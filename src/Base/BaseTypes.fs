module ServiceFabricMath.Base.BaseTypes

[<Struct>]
/// Abstract datatype for elements with an invertible group operation. 
/// This is a struct and really shouldn't be used for anything bigger than, say, a complex Decimal.
type SmallGroupElement< ^T when 
                    ^T : (static member (*) : ^T * ^T -> ^T) and 
                    ^T : (static member (/) : ^T * ^T -> ^T) and
                    ^T : (static member One : ^T) and 
                    ^T : equality > =
        | Val of ^T

/// Abstract datatype for elements with an invertible group operation. 
/// This is a reference type and really shouldn't be used for anything that could fit in a struct - e.g. a matrix or a function. 
type BigGroupElement< ^T when 
                    ^T : (static member (*) : ^T * ^T -> ^T) and 
                    ^T : (static member (/) : ^T * ^T -> ^T) and
                    ^T : (static member One : ^T) and 
                    ^T : equality > =
        | Val of ^T

[<Struct>]
/// Abstract datatype for elements with invertible addition and multiplication. 
/// This is a struct and really shouldn't be used for anything bigger than, say, a complex Decimal.
type SmallFieldElement< ^T when 
                 ^T : (static member (+) : ^T * ^T -> ^T ) and 
                 ^T : (static member (-) : ^T * ^T -> ^T ) and 
                 ^T : (static member (*) : ^T * ^T -> ^T) and 
                 ^T : (static member (/) : ^T * ^T -> ^T) and
                 ^T : (static member Zero : ^T) and 
                 ^T : equality > =
        | Val of ^T


/// Abstract datatype for elements with invertible addition and multiplication. 
/// This is a reference type and really shouldn't be used for anything that could fit in a struct - e.g. a matrix or a function. 
type BigFieldElement< ^T when 
                 ^T : (static member (+) : ^T * ^T -> ^T ) and 
                 ^T : (static member (-) : ^T * ^T -> ^T ) and 
                 ^T : (static member (*) : ^T * ^T -> ^T) and 
                 ^T : (static member (/) : ^T * ^T -> ^T) and
                 ^T : (static member Zero : ^T) and 
                 ^T : equality > =
        | Val of ^T

/// Type alias for column vectors.
type BasicColumnVector< ^T> = ^T array

/// Type alias for row vectors.
type BasicRowVector< ^T> = ^T array