/*
 * Copyright (C)2005-2016 Haxe Foundation
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

// This file is generated from mozilla/AudioNode.webidl line 24:0. Do not edit!

package js.html.audio;

@:native("AudioNode")
extern class AudioNode extends js.html.EventTarget
{
	var context(default,null) : AudioContext;
	var numberOfInputs(default,null) : Int;
	var numberOfOutputs(default,null) : Int;
	var channelCount : Int;
	var channelCountMode : ChannelCountMode;
	var channelInterpretation : ChannelInterpretation;
	
	/** @throws DOMError */
	@:overload( function( destination : AudioNode, ?output : Int = 0, ?input : Int = 0 ) : Void {} )
	function connect( destination : AudioParam, ?output : Int = 0 ) : Void;
	/** @throws DOMError */
	function disconnect( ?output : Int = 0 ) : Void;
}