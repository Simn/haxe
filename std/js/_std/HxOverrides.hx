/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
@:noDoc
class HxOverrides {

	static function dateStr( date :Date ) : String {
		var m = date.getMonth() + 1;
		var d = date.getDate();
		var h = date.getHours();
		var mi = date.getMinutes();
		var s = date.getSeconds();
		return date.getFullYear()
			+"-"+(if( m < 10 ) "0"+m else ""+m)
			+"-"+(if( d < 10 ) "0"+d else ""+d)
			+" "+(if( h < 10 ) "0"+h else ""+h)
			+":"+(if( mi < 10 ) "0"+mi else ""+mi)
			+":"+(if( s < 10 ) "0"+s else ""+s);
	}

	static function strDate( s : String ) : Date {
		switch( s.length ) {
		case 8: // hh:mm:ss
			var k = s.split(":");
			var d : Date = untyped __new__(Date);
			untyped d["setTime"](0);
			untyped d["setUTCHours"](k[0]);
			untyped d["setUTCMinutes"](k[1]);
			untyped d["setUTCSeconds"](k[2]);
			return d;
		case 10: // YYYY-MM-DD
			var k = s.split("-");
			return new Date(cast k[0],cast untyped k[1] - 1,cast k[2],0,0,0);
		case 19: // YYYY-MM-DD hh:mm:ss
			var k = s.split(" ");
			var y = k[0].split("-");
			var t = k[1].split(":");
			return new Date(cast y[0],cast untyped y[1] - 1,cast y[2],cast t[0],cast t[1],cast t[2]);
		default:
			throw "Invalid date format : " + s;
		}
	}

	static function cca( s : String, index : Int ) : Null<Int> {
		#if mt
		var x = (cast s).cca(index);
		#else
		var x = (cast s).charCodeAt(index);
		#end
		if( x != x ) // fast isNaN
			return untyped undefined; // isNaN will still return true
		return x;
	}

	static function substr( s : String, pos : Int, ?len : Int ) : String {
		if( pos != null && pos != 0 && len != null && len < 0 ) return "";
		if( len == null ) len = s.length;
		if( pos < 0 ){
			pos = s.length + pos;
			if( pos < 0 ) pos = 0;
		}else if( len < 0 ){
			len = s.length + len - pos;
		}

		return (untyped s).substr(pos, len);
	}

	// Array extensions

	static function arrayCopy<T>(a:Array<T>) {
		return (untyped a).slice();
	}

	static function arrayIndexOf<T>( a : Array<T>, obj : T, i : Int) {
		var len = a.length;
		if (i < 0) {
			i += len;
			if (i < 0) i = 0;
		}
		while (i < len)
		{
			if (untyped __js__("a[i] === obj"))
				return i;
			i++;
		}
		return -1;
	}

	static function arrayInsert<T>( a: Array<T>, pos : Int, obj : T) {
		(untyped a).splice(pos, 0, obj);
	}

	static function arrayIterator<T>( a : Array<T> ) : Iterator<T> untyped {
		return {
			cur : 0,
			arr : a,
			hasNext : function() {
				return __this__.cur < __this__.arr.length;
			},
			next : function() {
				return __this__.arr[__this__.cur++];
			}
		};
	}

	static function arrayLastIndexOf<T>( a : Array<T>, obj : T, i : Int) {
		var len = a.length;
		if (i >= len)
			i = len - 1;
		else if (i < 0)
			i += len;
		while (i >= 0)
		{
			if (untyped __js__("a[i] === obj"))
				return i;
			i--;
		}
		return -1;
	}

	static function arrayRemove<T>( a : Array<T>, obj : T ) {
		var i = a.indexOf(obj);
		if( i == -1 ) return false;
		a.splice(i,1);
		return true;
	}

	// Runtime resolvers

	@:ifFeature("dynamic.insert")
	@:runtime
	static function copy(d:Dynamic) {
		if (Std.is(d, Array)) {
			return function() {
				return arrayCopy(d);
			}
		} else {
			return untyped d["insert"];
		}
	}

	@:ifFeature("dynamic.insert")
	@:runtime
	static function insert(d:Dynamic) {
		if (Std.is(d, Array)) {
			return function(pos, obj) {
				return arrayInsert(d, pos, obj);
			}
		} else {
			return untyped d["insert"];
		}
	}

	@:ifFeature("dynamic.iterator")
	@:runtime
	static function iterator(d:Dynamic) {
		if (Std.is(d, Array)) {
			return function() {
				return HxOverrides.arrayIterator(d);
			}
		} else if (untyped __js__("typeof(d.iterator) == 'function'")) {
			return untyped __js__("$bind(d, d.iterator)");
		} else {
			return untyped d["iterator"];
		}
	}

	@:ifFeature("dynamic.remove")
	@:runtime
	static function remove(d:Dynamic) {
		if (Std.is(d, Array)) {
			return arrayRemove.bind(d);
		} else {
			return untyped d["remove"];
		}
	}

	#if !js_es5

	@:ifFeature("dynamic.indexOf")
	@:runtime
	static function indexOf(d:Dynamic) {
		if (Std.is(d, Array)) {
			return function(obj, ?fromIndex) {
				return arrayIndexOf(d, obj, (fromIndex!=null)?fromIndex:0);
			}
		} else {
			return untyped d["indexOf"];
		}
	}

	@:ifFeature("dynamic.lastIndexOf")
	@:runtime
	static function lastIndexOf(d:Dynamic) {
		if (Std.is(d, Array)) {
			return function(obj, ?fromIndex) {
				return arrayLastIndexOf(d, obj, (fromIndex!=null)?fromIndex:0);
			}
		} else {
			return untyped d["lastIndexOf"];
		}
	}

	#end
}
