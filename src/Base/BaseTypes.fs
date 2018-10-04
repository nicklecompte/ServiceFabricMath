module ServiceFabricMath.Base.BaseTypes

open Contracts

[<Struct>]
/// Abstract datatype for elements with a binary operation. 
/// This is a struct and really shouldn't be used for anything bigger than, say, a complex Decimal.
type SmallMagmaElement< ^T when 
                    ^T : (static member (*) : ^T * ^T -> ^T) and 
                    ^T : equality > =
        | SMEVal of ^T
with
        member inline __.GetProd x y = x * y         
        interface IMagma< ^T> with
                member x.BinaryOp x y = x.GetProd x y

/// Abstract datatype for elements with a binary operation. 
/// This is a reference type and should be used for anything that can't fit in a struct - e.g. a matrix or a function. 
type BigMagmaElement< ^T when 
                    ^T : (static member (*) : ^T * ^T -> ^T) and 
                    ^T : (static member One : ^T) and 
                    ^T : equality > =
        | Val of ^T

[<Struct>]
/// Abstract datatype for elements with a binary operation and identity. 
/// This is a struct and really shouldn't be used for anything bigger than, say, a complex Decimal.
type SmallMonoidlement< ^T when 
                    ^T : (static member (*) : ^T * ^T -> ^T) and 
                    ^T : (static member One : ^T) and 
                    ^T : equality > =
        | Val of ^T

/// Abstract datatype for elements with a binary operation and identity. 
/// This is a reference type and should be used for anything that can't fit in a struct - e.g. a matrix or a function. 
type BigMonoidElement< ^T when 
                    ^T : (static member (*) : ^T * ^T -> ^T) and 
                    ^T : (static member One : ^T) and 
                    ^T : equality > =
        | Val of ^T

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
/// This is a reference type and should be used for anything that can't fit in a struct - e.g. a matrix or a function. 
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
/// This is a reference type and really should be used for anything that can't fit in a struct - e.g. a matrix or a function. 
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