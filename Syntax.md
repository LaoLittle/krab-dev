
```
Program := Package? Import* Stmt*
Stmt := Decl | Expr | While | For | Assign

Decl := VariableDecl | FunDecl
VariableDecl := ('val' | 'var') id (':' Type)? ('=' Expr)?
FunDecl := Vis? 'fun' id (GenericList)? '(' (ArgDecl ','?)* ')' '{' Stmt* '}'

ArgDecl := id ':' Type
CtorArgDecl := ('val' | 'var')? ArgDecl

StructDecl := (Vis)? 'struct' id ('(' (CtorArgDecl ','?)* ')')? '{' Decl* '}'
ClassDecl := (Vis)? 'class' id ('(' (CtorArgDecl ','?)* ')')? '{' (Decl | InitBlock)* '}'

InitBlock := 'init' '{' Stmt* '}'

GenericList := '<' (id ','?)+ '>'
Type := id (GenericList)? '?'?
Annotation := '@' id ('(' (Expr ','?)+ ')')?
Vis := 'public' | 'private' | 'internal'
```

# Custom types
```kotlin
@Represent(.C) // represents a C struct
@Alignment(16)
struct Struct(val a: Type, b: Type) { // structs are copyable and cannot be allocated on heap directly.
    val field1 = 1
    
    fun pubFun() {} // same as 'public fun pubFun() {}'
    private fun priFun<T>() {}
}

class Class(val a: Type) {
    val s: Struct // allocate a struct inside a class. Structs are non-nullable.
    
    
    init {
        s = Struct()
    }
}
```