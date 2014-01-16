package unit;

using unit.MyTypeConstructor;

class TestTypeConstructor extends Test {
	public function testMonadTransformers() 
	{
		var func = new ArrayTFunctor(new ArrayTFunctor(new EitherMonad()));
		var e = Right([[1]]);

		var r = e.arrayT().arrayT().fmap(function (x) return x+1, func).runT().runT();

		
		var rx : Either<String, Array<Array<Int>>> = r;
		t(r.match(Right([[2]])));

		var r = e.fmap(function (x) return 1, new EitherMonad());

		t(r.match(Right(1)));		

		var r = e.arrayT().fmap(function (x) return [1], new ArrayTFunctor(new EitherMonad())).runT();

		t(r.match(Right([[1]])));

		var r = e.arrayT().arrayT().fmap(function (x) return x+1, new ArrayTFunctor(new ArrayTFunctor(new EitherMonad()))).runT().runT();

		t(r.match(Right([[2]])));

		var r = e.arrayT().arrayT().fmap(function (x) return x+1, func).runT().runT();

		t(r.match(Right([[2]])));

		

		var e2 = Right([[Right(1)]]);

		

		var r = e2.arrayT().arrayT().eitherT().fmap(function (x) return x+1, new EitherTFunctor(new ArrayTFunctor(new ArrayTFunctor(new EitherMonad())))).runT().runT().runT();

		
		
		t(r.match(Right([[Right(2)]])));


	}

	public function testLeftInType() 
	{
		var x : Of<Either<In, Int>, String> = Left("hello");

		//$type(x.fmap);

		var r = x.fmap(function (x) return x+" world", new EitherLeftMonad());

		t(r.match(Left("hello world")));

		var r = x.flatMap(function (x) return Left(x+" world"), new EitherLeftMonad());

		t(r.match(Left("hello world")));
	}

	public function testReversedTypedef() 
	{
		var x : Of<ReversedEitherAlias<Int, In>, String> = Left("hello");
		
		var r = x.fmap(function (x) return x+" world", new EitherLeftMonad());

		t(r.match(Left("hello world")));

		var r = x.flatMap(function (x) return Left(x+" world"), new EitherLeftMonad());

		t(r.match(Left("hello world")));

	}

	public function testStructure() 
	{
		// explicit left lifted Of type

		var z:Of<Tup2<In,Int>, String> = { _1 : "hey", _2 : 20};

		var r = z.fmap(function (x) return x + " foo", new Tup2LeftFunctor());

		t(r._1 == "hey foo" && r._2 == 20);

		// explicit right lifted Of type

		var z:Of<Tup2<String,In>, Int> = { _1 : "hey", _2 : 20};

		var r = z.fmap(function (x) return x + 20, new Tup2RightFunctor());

		t(r._1 == "hey" && r._2 == 40);


		// implicit Of type: always the right type parameter is lifted

		var z:Tup2<String,Int> = { _1 : "hey", _2 : 20};

		var r = z.fmap(function (x) return x + 20, new Tup2RightFunctor());

		t(r._1 == "hey" && r._2 == 40);


	}

	public function testReversedAbstract() 
	{
		var x : ReversedEither<Int, String> = new ReversedEither(Left("hello"));

		var r = x.fmap(function (x) return x+" world", new ReversedEitherMonad());

		t(r.toEither().match(Left("hello world")));
	}


	public function testMappable() {
		
		function mapMappable <M,A,B>(m:Mappable<M,A>, f:A->B):M<B> {
			return m.map(f);
		}

		var x = new MyArray([1]);
		
		var r = mapMappable(x, function (y) return y+1);

		t(r.a[0] == 2);

		var l = new List();
		l.add(1);
		var x = new MyList(l);

		var r = mapMappable(x, function (y) return y+1);

		t(r.a.first() == 2);

	}

	

	public function testFlatMappable() {

		function flatMapFlatMappable <M,A,B>(m:FlatMappable<M,A>, f:A->M<B>):M<B> {
			return m.flatMap(f);
		}

		var x = new MyArray([1]);
		
		var r = flatMapFlatMappable(x, function (y) return new MyArray([y+1]));

		t(r.a[0] == 2);

		var l = new List();
		l.add(1);
		var x = new MyList(l);

		var r = flatMapFlatMappable(x, function (y) return new MyList({ var l = new List(); l.add(y+1); l;}));

		t(r.a.first() == 2);


	}
	
	

	public function testMappableWithTypedef() {

		function mapMappable <M,A,B>(m:MappableTD<M,A>, f:A->B):M<B> {
			// you can currently not apply map again (like m.map(f).map(f)), this is a limitation 
			// which could be solved with a proper constraint on MappableTD, a solution how they can be applied on of types is  described at the bottom.
			return m.map(f);
		}
		var x = [1];
		
		var r = mapMappable(x, function (y) return y+1);

		t(r[0] == 2);

		var x = new List();
		x.add(1);
		
		var r = mapMappable(x, function (y) return y+1);

		t(r.first() == 2);

	}


	

	public function testFilterableWithTypedef() {
		
		function filterFilterable <M,A,B>(m:Filterable<M,A>, f:A->Bool):M<A> {
			// you can currently not apply filter again, this is a limitation which could be solved with constraints described at the bottom
			return m.filter(f);
		}
		var x = [1,2,3,4];
		
		var r = filterFilterable(x, function (y) return y > 2);
		t(r.length == 2);
		t(r[0] == 3);
		t(r[1] == 4);
		var x = new List();
		x.add(1);
		x.add(2);
		x.add(3);
		x.add(4);
		
		var r = filterFilterable(x, function (y) return y > 2);

		t(r.length == 2);
		t(r.first() == 3);
		t(r.last() == 4);
	}

	
 	
 	/*
 	
 	// this is currently not working, but it would be very important
 	// it should work as follows if you found an of type like M<T> (Of<M, T>) inside of the 
 	// function body, you have to look at it's constraints and replace the In
 	// type in each constraint with the type T.
 	// In case of Filterable<M,In> you can just follow Of<Filterable<M, In>, T> it should be quite simple.


 	private static function typeConstructorWithConstraints <M:(Filterable<M,In>, MappableTD<M,In>)> (m:M<T>):M<T> 
 	{
 		return m.map(function (x) return x+1).filter(function (x) return x > 2);
	}
	
	*/

}