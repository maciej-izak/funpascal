This repository contains an experimental, very simple Pascal compiler created in F# called FunPascal (short name : fnp). FunPascal in its current stage is very limited but it works and it can compile another Pascal compiler called XDPW (and this compiled compiler in FunPascal compiler works well). I was very interested how will looks like simple compiler created by using existing libraries like FParsec and dnlib in F#. The result IMO is not bad. Main purpose of this project was learning F#, which was an amazing experience. When I started, I had an extreme problem compiling even simple functional F# code just to print some values on the screen (sic!), but with a lot of patience and "unlearning" imperative/OO programming skills, the real fun with F# started. I feel much more free. Thanks to F# I am a better C# programmer, much more open minded. 

Some notes:

* At the moment the compiler uses RTL from XD Pascal.
* In the future I had planned to use part (or even large part) of RTL from FreePascal
* The main goal of this project is fun(ctional) programming and border crossing :)!
* One of the goals is to create a compiler more like C++/CLI which should be able to compile most of existing Pascal code (like Lazarus and mORMot code base) and additionally use of .NET. This will not be pure .NET oriented compiler.
* At some point I want to implement mselang dialect, but priority is Delphi mode from FreePascal
* This is a long term never ending project, for now the main problem is time. If you like my idea you can sponsor me (or not).
* License of this project is GPL v3 but probably will be changed to more liberal MIT

The run of the compiler may be real pain, it has no build scripts yet, and source code has few hardcoded paths. This was created as a pure hobby project for fun. I decided to make it public to get more self motivation. "running compiler" experience should improve in time.

If you want to have an adventure, and you want to test compiler you need to download win-x86 .NET runtime, but probably a better idea is to wait a bit for building scripts (see issue #1) and generic version of project (issue #2).