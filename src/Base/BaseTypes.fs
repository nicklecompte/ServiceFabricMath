module ServiceFabricMath.Base.BaseTypes

#nowarn "193"

[<Struct>]
/// Generic complex numbers
type Complex< ^T when 
        ^T : (static member (+) :  ^T * ^T -> ^T ) and 
        ^T : (static member (-) : ^T * ^T -> ^T ) and 
        ^T : (static member (*) : ^T * ^T -> ^T) and 
        ^T : (static member (/) : ^T * ^T -> ^T) and
        ^T : (static member Zero : ^T) and 
        ^T : (static member Sqrt : ^T -> ^T) and
        ^T : struct and
        ^T : equality> = {
                real: ^T
                imaginary: ^T
        }
with
        member inline x.Conjugate = {real=x.real;imaginary = LanguagePrimitives.GenericZero< ^T> - x.imaginary}
        static member inline (+) ((a:Complex< ^T>),(b:Complex< ^T>)) = 
                {real = a.real+b.real; imaginary = a.imaginary+b.imaginary}

        static member inline (*) ((a:Complex< ^T>),(b:Complex< ^T>)) =
                {real = a.real*a.real - (b.imaginary*b.imaginary); imaginary = (a.imaginary*a.real)+(a.imaginary*a.real)}