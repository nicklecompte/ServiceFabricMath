module ServiceFabricMath.Common.Complex

//let inline getTwo< ^T> () : ^T = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne

type Complex< ^T when
        ^T: (static member (+) : ^T * ^T -> ^T) and
        ^T: (static member (*) : ^T * ^T -> ^T) and
        ^T: (static member (-) : ^T * ^T -> ^T) and
        ^T: (static member (/) : ^T * ^T -> ^T) and
        ^T: (static member One: ^T) and
        ^T: (static member Zero: ^T)> = {
    real: ^T
    complex: ^T
}
with
    static member inline Zero: Complex< ^T> = {real = LanguagePrimitives.GenericZero; complex = LanguagePrimitives.GenericZero}
    static member inline One: Complex< ^T> = {real = LanguagePrimitives.GenericOne; complex = LanguagePrimitives.GenericZero}
    static member inline i: Complex< ^T> = {real = LanguagePrimitives.GenericZero; complex = LanguagePrimitives.GenericOne}
    static member inline (+) (a,b) = {real = a.real + b.real; complex = a.complex + b.complex}
    static member inline (-) (a,b) = {real = a.real - b.real; complex = a.complex - b.complex}
    /// Conjugate.
    static member inline conj a = {real = a.real; complex = LanguagePrimitives.GenericZero - a.complex}
    static member inline (*) (a,b) = 
        {real = a.real*b.real - a.complex*b.complex; complex = a.real * b.complex + a.complex*b.real}
    static member inline squaredModulus a : ^T = a.real*a.real + a.complex*a.complex
    static member inline (/) (a,b) = 
        let sqModB = Complex.squaredModulus b
        {real = (a.real*b.real + a.complex*b.complex)/sqModB; complex = (a.complex*b.real - a.real*b.complex)/sqModB}