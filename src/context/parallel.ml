let enable = ref true

(* Minimum number of items to justify spawning domains.
   Below this threshold we run sequentially on the calling domain. *)
let min_parallel_items = 8

let num_domains () =
	Domain.recommended_domain_count ()

(* ──────────────────────────────────────────────────────────────────────
   WorkerPool — a reusable pool of OS domains that sleep (via
   Condition.wait, zero CPU) between parallel_for calls.

   Workers are spawned once when the pool is created and joined when it
   is torn down.  Between calls they block on a condition variable —
   no spin-wait, no OS thread creation/destruction per call.

   Nested parallel_for calls (e.g. iterating over types, where each
   type iterates over fields) are detected via an atomic [busy] flag
   and fall back to sequential execution.
   ────────────────────────────────────────────────────────────────────── *)
module WorkerPool = struct
	type t = {
		(* Total domains = num_workers + 1 (main). *)
		num_workers : int;
		(* Dispatch a parallel-for: [submit length f] calls [f 0] … [f (length-1)]. *)
		submit : int -> (int -> unit) -> unit;
		(* Tear down workers (join). *)
		shutdown : unit -> unit;
	}

	(* Create a pool with [nw] worker domains (0 = sequential). *)
	let create nw =
		if nw <= 0 then {
			num_workers = 0;
			submit = (fun length f -> for i = 0 to length - 1 do f i done);
			shutdown = (fun () -> ());
		}
		else begin
			(* -- shared mutable state, all protected by [mu] -- *)
			let mu           = Mutex.create () in
			let work_avail   = Condition.create () in
			let all_done     = Condition.create () in
			let body         = ref (fun (_:int) -> ()) in
			let chunk_starts = Array.make nw 0 in
			let chunk_ends   = Array.make nw (-1) in (* -1 = empty range *)
			let gen          = ref 0 in
			let remaining    = ref 0 in
			let stop         = ref false in
			(* First exception (with backtrace) from any domain. *)
			let exc : (exn * Printexc.raw_backtrace) option Atomic.t = Atomic.make None in
			(* True while a parallel_for is in progress.  Nested calls
			   (from within the body of a parallel_for) fall back to
			   sequential to avoid corrupting the pool's shared state. *)
			let busy = Atomic.make false in

			(* Worker loop — runs on each spawned domain. *)
			let worker idx =
				let my_gen = ref 0 in
				let running = ref true in
				while !running do
					Mutex.lock mu;
					while !gen = !my_gen && not !stop do
						Condition.wait work_avail mu
					done;
					if !stop then begin
						Mutex.unlock mu;
						running := false
					end else begin
						my_gen := !gen;
						let f = !body in
						let s = chunk_starts.(idx) in
						let e = chunk_ends.(idx) in
						Mutex.unlock mu;
						(* Execute the assigned chunk. *)
						(try for j = s to e do f j done
						 with exn ->
							let bt = Printexc.get_raw_backtrace () in
							ignore (Atomic.compare_and_set exc None (Some (exn, bt))));
						(* Signal completion. *)
						Mutex.lock mu;
						decr remaining;
						if !remaining = 0 then Condition.signal all_done;
						Mutex.unlock mu
					end
				done
			in

			let workers = Array.init nw (fun idx ->
				Domain.spawn (fun () -> worker idx)
			) in

			let nd = nw + 1 in (* total domains *)

			let submit length f =
				if length <= 0 then ()
				else if length < min_parallel_items || not (Atomic.compare_and_set busy false true) then
					(* Too few items or nested call — run sequentially. *)
					for i = 0 to length - 1 do f i done
				else begin
					let n  = min nd length in
					let chunk = length / n in
					let rem   = length mod n in
					(* Prepare worker chunks (workers process chunks 1 … n-1). *)
					Mutex.lock mu;
					body := f;
					Atomic.set exc None;
					for i = 0 to nw - 1 do
						let ci = i + 1 in
						if ci < n then begin
							chunk_starts.(i) <- ci * chunk + min ci rem;
							chunk_ends.(i)   <- (ci + 1) * chunk + min (ci + 1) rem - 1
						end else begin
							chunk_starts.(i) <- 0;
							chunk_ends.(i)   <- -1 (* empty range — finishes instantly *)
						end
					done;
					remaining := nw;
					incr gen;
					Condition.broadcast work_avail;
					Mutex.unlock mu;
					(* Main domain processes chunk 0. *)
					let e0 = chunk + (if rem > 0 then 1 else 0) - 1 in
					(try for j = 0 to e0 do f j done
					 with exn ->
						let bt = Printexc.get_raw_backtrace () in
						ignore (Atomic.compare_and_set exc None (Some (exn, bt))));
					(* Wait for all workers to finish. *)
					Mutex.lock mu;
					while !remaining > 0 do
						Condition.wait all_done mu
					done;
					Mutex.unlock mu;
					Atomic.set busy false;
					(* Re-raise the first captured exception, if any. *)
					match Atomic.get exc with
					| Some (exn, bt) -> Printexc.raise_with_backtrace exn bt
					| None -> ()
				end
			in

			let shutdown () =
				Mutex.lock mu;
				stop := true;
				Condition.broadcast work_avail;
				Mutex.unlock mu;
				Array.iter Domain.join workers
			in

			{ num_workers = nw; submit; shutdown }
		end

	let parallel_for pool length f = pool.submit length f
	let teardown pool = pool.shutdown ()
end

(* ──────────────────────────────────────────────────────────────────────
   Public API — kept identical to the old Domainslib-based API so that
   callers (filters.ml, genjvm.ml, etc.) need minimal changes.
   The pool token is now [WorkerPool.t option] instead of
   [Domainslib.Task.pool option].
   ────────────────────────────────────────────────────────────────────── *)

let run_parallel_for nd ?(chunk_size=0) length f =
	ignore chunk_size; (* kept for API compat *)
	if not !enable then
		for i = 0 to length - 1 do f i done
	else begin
		let wp = WorkerPool.create (nd - 1) in
		Std.finally (fun () -> WorkerPool.teardown wp) (WorkerPool.parallel_for wp length) f
	end

module ParallelArray = struct
	(** [iter pool f a]: iterate [f] over [a] in parallel when [pool = Some wp]. *)
	let iter pool f a =
		match pool with
		| None ->
			Array.iter f a
		| Some wp ->
			WorkerPool.parallel_for wp (Array.length a) (fun i -> f a.(i))

	(** [map pool f a default]: map [f] over [a] in parallel when [pool = Some wp]. *)
	let map pool f a x =
		match pool with
		| None ->
			Array.map f a
		| Some wp ->
			let length = Array.length a in
			let a_out = Array.make length x in
			WorkerPool.parallel_for wp length (fun i ->
				Array.unsafe_set a_out i (f (Array.unsafe_get a i))
			);
			a_out
end

module ParallelSeq = struct
	let iter pool f seq =
		ParallelArray.iter pool f (Array.of_seq seq)
end

(* A managed pool that lazily creates a WorkerPool on first use and
   keeps it alive (workers sleeping via Condition.wait — zero CPU)
   until [release] is called.  This matches the old Domainslib lifecycle. *)
module ManagedPool = struct
	type t = {
		num_domains : int;
		mutable pool : WorkerPool.t option;
		mutex : Mutex.t;
	}

	let create () = {
		num_domains = Domain.recommended_domain_count ();
		pool = None;
		mutex = Mutex.create ();
	}

	let acquire mp =
		Mutex.protect mp.mutex (fun () ->
			match mp.pool with
			| Some wp -> wp
			| None ->
				let wp = WorkerPool.create (mp.num_domains - 1) in
				mp.pool <- Some wp;
				wp
		)

	let release mp =
		Mutex.protect mp.mutex (fun () ->
			match mp.pool with
			| Some wp ->
				WorkerPool.teardown wp;
				mp.pool <- None
			| None -> ()
		)

	let is_active mp =
		Mutex.protect mp.mutex (fun () ->
			mp.pool <> None
		)
end

let run_in_new_pool timer_ctx f =
	if not !enable then
		f None
	else begin
		let nd = Timer.time timer_ctx ["parallel";"setup"] num_domains () in
		let wp = WorkerPool.create (nd - 1) in
		Std.finally (fun () -> WorkerPool.teardown wp) f (Some wp)
	end

let run_with_pool mp f =
	if not !enable then
		f None
	else
		let wp = ManagedPool.acquire mp in
		f (Some wp)
