# Single-threaded linear algebra modules

The purpose of these modules is to do "smaller" linear algebra modules on a single thread. These make use of mutability for performance, including mutable properties/methods from System.Collections.Generic.

Type breakdown:

* ColumnVector< ^T> and RowVector< ^T>