Things to do
============

- [ ] Call _delete when the scope of an array ends.
- [x] Change intmb to int option.
- [x] Make quads consequent.
- [ ] Check if delete is performed on objects created by new
			   Consider leaving this as it is. Things that does not work:
					* Different data type (T_Ref of typ * allocmode) with default mode:
					User type annotations (being default) failed.
					* Different data type (T_Ref of typ * allocmode) with a fresh constuctor:
					The same function cannot be used with mutable objects and objects created with new.
- [ ] Offsets: symbol table??
- [ ] Fun/Obj
