package unit;


using unit.MyTypeConstructor.ArrayT;
using unit.MyTypeConstructor.EitherT;

enum Either<L,R> {
  Left(l:L);
  Right(r:R);
}

interface Functor<F> {
  public function fmap<A,B>(x:F<A>, f:A->B):F<B>;
}

interface Monad<M> extends Functor<M> {
   public function flatMap<A,B>(x:M<A>, f:A->M<B>):M<B>;
}



class EitherMonad<L> implements Monad<Either<L, In>>
{
     public function new () {}
     public function fmap<A,B>(x:Either<L, A>, f:A->B):Either<L, B> {
         return switch (x) {
            case Right(r): Right(f(r));
            case Left(l): Left(l);
         }
     }
     public function flatMap<A,B>(x:Either<L, A>, f:A->Either<L,B>):Either<L, B> {
         return switch (x) {
            case Right(r): f(r);
            case Left(l): Left(l);
         }
     }
}


class EitherLeftMonad<R> implements Monad<Either<In, R>> 
{
     public function new () {}
     public function fmap<A,B>(x:Either<A, R>, f:A->B):Either<B, R> {
         return switch (x) {
            case Right(r): Right(r);
            case Left(l): Left(f(l));
         }
     }
     public function flatMap<A,B>(x:Either<A, R>, f:A->Either<B,R>):Either<B, R> {
         return switch (x) {
            case Right(r): Right(r);
            case Left(l): f(l);
         }
     }
}


class ReversedEitherMonad<R> implements Monad<ReversedEither<R, In>> 
{
     public function new () {}
     public function fmap<A,B>(x:ReversedEither<R, A>, f:A->B):ReversedEither<R, B> {
         return switch (x.toEither()) {
            case Right(r): new ReversedEither(Right(r));
            case Left(l): new ReversedEither(Left(f(l)));
         }
     }
     public function flatMap<A,B>(x:ReversedEither<R, A>, f:A->ReversedEither<R,B>):ReversedEither<R, B> {
         return switch (x.toEither()) {
            case Right(r): new ReversedEither(Right(r));
            case Left(l): f(l);
         }
     }
}


 
class ArrayMonad implements Monad<Array<In>> {
  public function new () {}
  public inline function fmap<S,T>(a:Array<S>, f:S->T):Array<T> {
    return a.map(f);
  }
  public inline function flatMap<S,T>(a:Array<S>, f:S->Array<T>):Array<T> {
    return [for (x in a) for (y in f(x)) y];
  }
}


abstract ArrayT<M, A>(M<Array<A>>) 
{
  public function new (x:M<Array<A>>) {
    this = x;
  }
  public function unwrap ():M<Array<A>> {
    return this;
  }

  public static function runT <M1,A1>(a:ArrayT<M1,A1>):M1<Array<A1>> 
  {
    return a.unwrap();
  }
  public static function arrayT <M1,A1>(a:M1<Array<A1>>):ArrayT<M1,A1>
  {
    return new ArrayT(a);
  }
}

abstract EitherT<M,L,A>(M<Either<L,A>>) 
{
  public function new (x:M<Either<L,A>>) {
    this = x;
  }
  public function unwrap ():M<Either<L,A>> {
    return this;
  }

  public static function runT <M1,L,A1>(a:EitherT<M1,L,A1>):M1<Either<L,A1>> 
  {
    return a.unwrap();
  }
  public static function eitherT <M,L,A>(a:M<Either<L, A>>):EitherT<M,L,A>
  {
    return new EitherT(a);
  }
}


class ArrayTFunctor<M> implements Functor<ArrayT<M,In>> 
{
  var functorM:Functor<M>;
  
  public function new (functorM:Functor<M>) 
  {
    this.functorM = functorM;
  }
  
  public function fmap<A,B>(v:ArrayT<M, A>,f:A->B):ArrayT<M, B>
  {
    function g (a:Array<A>) return a.map(f);

    return functorM.fmap(v.runT(), g).arrayT();
  }

}

class EitherTFunctor<M,L> implements Functor<EitherT<M,L,In>> 
{
  var functorM:Functor<M>;
  
  public function new (functorM:Functor<M>) 
  {
    this.functorM = functorM;
  }
  
  public function fmap<A,B>(v:EitherT<M,L,A>,f:A->B):EitherT<M,L,B>
  {
    function g (a:Either<L,A>) 
      return switch (a) {
        case Left(x): Left(x);
        case Right(x): Right(f(x));
      }

    return functorM.fmap(v.runT(), g).eitherT();
  }

}


class FunctorSyntax {
  public static function fmap<M, S, T>(x:M<S>, f:S->T, m:Functor<M>):M<T> 
  {
    return m.fmap(x, f);
  }
}
class MonadSyntax {
  public static function flatMap<M, S, T>(x:M<S>, f:S->M<T>, m:Monad<M>):M<T> 
  {
    return m.flatMap(x, f);
  }
}


typedef ReversedEitherAlias<B,A> = Either<A,B>;

abstract ReversedEither<R,L>(Either<L,R>) 
{
  public function new (x:Either<L,R>) this = x;

  public function toEither ():Either<L,R> {
    return this;
  }
}

typedef Tup2<A,B> = { var _1 : A; var _2 : B; }

class Tup2LeftFunctor<T> implements Functor<Tup2<In, T>> 
{
  public function new () {}
  
  public inline function fmap<A,B>(a:Tup2<A, T>, f:A->B):Tup2<B, T> {
    return {
      _1 : f(a._1),
      _2 : a._2,
    }
  }

}

class Tup2RightFunctor<T> implements Functor<Tup2<T, In>> {
  public function new () {}
  public inline function fmap<A,B>(a:Tup2<T, A>, f:A->B):Tup2<T, B> 
  {
    return {
      _1 : a._1,
      _2 : f(a._2),
    }
  }  
}

interface Mappable<M,T> {
  public function map <B>(f:T->B):M<B>;
}

typedef MappableTD<M,T> = {
  public function map <B>(f:T->B):M<B>;
}



interface FlatMappable<M,T> extends Mappable<M,T> {
  public function flatMap <B>(f:T->M<B>):M<B>;
}


class MyArray<T> implements FlatMappable<MyArray<In>, T> {
  public var a:Array<T>;
  public function new (a:Array<T>) {
    this.a = a;
  }
  public function map <B>(f:T->B):MyArray<B> {
    return new MyArray(this.a.map(f));
  }
  public function flatMap <B>(f:T->MyArray<B>):MyArray<B> {
    return new MyArray([for (x in a) for (y in f(x).a) y]);
  }
}


class MyList<T> implements FlatMappable<MyList<In>, T> {
  public var a:List<T>;
  public function new (a:List<T>) {
    this.a = a;
  }
  
  public function map <B>(f:T->B):MyList<B> {
    return new MyList(this.a.map(f));
  }

  public function flatMap <B>(f:T->MyList<B>):MyList<B> {
    var r = new List();
    for (x in a) for (y in f(x).a) r.add(y);
    return new MyList(r);
  }
}

typedef Filterable<M, T> = {
  public function filter (f:T->Bool):M<T>;  
}

/*
  supporting and applying type constraints like this correctly would be important, i already described how 
  this could be solved in TestTypeConstructor on the bottom.

typedef BetterFilterable<M:BetterFilterable<M,In>, T> = {
  public function filter (f:T->Bool):M<T>;  
}

*/