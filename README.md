# Oberon-compiler-with-generic-item-status-field
A modified Oberon-07 compiler which uses a generalized *x.status* field of type SET instead of a boolean *x.rdo* field for *items* (type *ORG.Item*).

This may be useful to track additional properties of items, e.g.,

*  bit 0 = readonly
*  bit 1 = dereferenced  (e.g. p^)
*  bit 2 = selected      (e.g. p.f)
*  ...

------------------------------------------------------
**DOWNLOADING AND BUILDING THE MODIFIED COMPILER**

Download the files from the [**Sources/ExtendedOberon**](Sources/ExtendedOberon) directory of this repository.

Convert the downloaded files to Oberon format (Oberon uses CR as line endings) using the command [**dos2oberon**](dos2oberon), also available in this repository (example shown for Mac or Linux):

     for x in *.Mod ; do ./dos2oberon $x $x ; done

Import the files to your Oberon system. If you use an emulator (e.g., **https://github.com/pdewacht/oberon-risc-emu**) to run the Oberon system, click on the *PCLink1.Run* link in the *System.Tool* viewer, copy the files to the emulator directory, and execute the following command on the command shell of your host system:

     cd oberon-risc-emu
     for x in *.Mod ; do ./pcreceive.sh $x ; sleep 1 ; done

Create the modified Oberon compiler:

     ORP.Compile ORS.Mod/s ORB.Mod/s ~
     ORP.Compile ORG.Mod/s ORP.Mod/s ~
     ORP.Compile ORL.Mod/s ORX.Mod/s ORTool.Mod/s ~
     System.Free ORTool ORP ORG ORB ORS ORL ORX ~

------------------------------------------------------
**DIFFERENCES TO EXTENDED OBERON**

**ORG.Mod**

```diff
--- Oberon-extended/Sources/ORG.Mod	2021-01-06 06:40:58.000000000 +0100
+++ Oberon-compiler-with-generic-item-status-field/Sources/ExtendedOberon/ORG.Mod	2021-01-12 11:35:58.000000000 +0100
@@ -5,6 +5,7 @@
      Procedure Close writes code-files*)
 
   CONST WordSize* = 4;
+    rdo* = 0; deref* = 1; selected* = 2;  (*item status*)
     StkOrg0 = -64; VarOrg0 = 0;  (*for RISC-0 only*)
     TR = 13; SP = 14; LNK = 15;  (*dedicated registers*)
     maxCode = 8800; maxStrx = 3200; maxTD = 160; maxSet = WordSize*8;
@@ -26,7 +27,7 @@
       mode*: INTEGER;
       type*: ORB.Type;
       a*, b*, r: LONGINT;
-      rdo*: BOOLEAN  (*read only*)
+      status*: SET
     END ;
     LabelRange* = RECORD low*, high*, label*: INTEGER END ;
 
@@ -293,7 +294,8 @@
   END MakeStringItem;
 
   PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object);
-  BEGIN x.mode := y.class; x.type := y.type; x.a := y.val; x.rdo := y.rdo;
+  BEGIN x.mode := y.class; x.type := y.type; x.a := y.val; x.status := {};
+    IF y.rdo THEN INCL(x.status, rdo) END ;
     IF y.class = ORB.Par THEN x.b := 0
     ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.r := y.lev;
       x.a := y.val MOD C20; (*strx/exno*) x.b := y.val DIV C20 (*len*)
@@ -304,7 +306,7 @@
   (* Code generation for Selectors, Variables, Constants *)
 
   PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
-  BEGIN
+  BEGIN EXCL(x.status, deref);
     IF x.mode = ORB.Var THEN
       IF x.r >= 0 THEN x.a := x.a + y.val
       ELSE loadAdr(x); x.mode := RegI; x.a := y.val
@@ -316,7 +318,7 @@
 
   PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
     VAR s, lim: LONGINT;
-  BEGIN s := x.type.base.size; lim := x.type.len;
+  BEGIN s := x.type.base.size; lim := x.type.len; EXCL(x.status, deref);
     IF (y.mode = ORB.Const) & (y.a < 0) THEN ORS.Mark("bad index") END ;
     IF (y.mode = ORB.Const) & (lim >= 0) THEN
       IF y.a >= lim THEN ORS.Mark("bad index") END ;
@@ -360,14 +362,19 @@
     ELSIF x.mode # Reg THEN ORS.Mark("bad mode in DeRef")
     END ;
     IF x.type.base.form = ORB.Array THEN Put1(Add, x.r, x.r, 16) END ; (*pointer to array*)
-    x.mode := RegI; x.a := 0; x.b := 0
+    x.mode := RegI; x.a := 0; x.b := 0; INCL(x.status, deref)
   END DeRef;
 
-  PROCEDURE Method*(VAR x: Item; y: ORB.Object; wasderef, super: BOOLEAN);
+  PROCEDURE Select*(VAR x: Item; sel: BOOLEAN);
+  BEGIN
+    IF sel THEN INCL(x.status, selected) ELSE EXCL(x.status, selected) END
+  END Select;
+
+  PROCEDURE Method*(VAR x: Item; y: ORB.Object; super: BOOLEAN);
   BEGIN loadAdr(x);  (*receiver*)
     IF super THEN x.a := y.val; (*mthadr/exno*) x.b := -y.type.mno (*global/imported, x.b <= 0*)
     ELSE x.a := y.lev; (*mthno*)
-      IF wasderef THEN x.b := ORB.Var (*pointer*) ELSE x.b := ORB.Par (*record*) END (*x.b > 0*)
+      IF deref IN x.status THEN x.b := ORB.Var (*pointer*) ELSE x.b := ORB.Par (*record*) END (*x.b > 0*)
     END
   END Method;
```

**ORP.Mod**

```diff
--- Oberon-extended/Sources/ORP.Mod	2021-01-05 11:28:44.000000000 +0100
+++ Oberon-compiler-with-generic-item-status-field/Sources/ExtendedOberon/ORP.Mod	2021-01-12 11:36:02.000000000 +0100
@@ -17,7 +17,6 @@
     dc: LONGINT;    (*data counter*)
     level, exno, version: INTEGER;
     newSF: BOOLEAN;  (*option flag*)
-    selected: BOOLEAN;  (*to detect whether a selection has been processed*)
     expression: PROCEDURE (VAR x: ORG.Item);  (*to avoid forward reference*)
     Type: PROCEDURE (VAR type: ORB.Type; expo: BOOLEAN);
     FormalType: PROCEDURE (VAR typ: ORB.Type; dim: INTEGER);
@@ -80,7 +79,7 @@
 
   PROCEDURE CheckReadOnly(VAR x: ORG.Item);
   BEGIN
-    IF x.rdo THEN ORS.Mark("read-only") END
+    IF ORG.rdo IN x.status THEN ORS.Mark("read-only") END
   END CheckReadOnly;
 
   PROCEDURE CheckExport(VAR expo: BOOLEAN);
@@ -92,19 +91,16 @@
     END
   END CheckExport;
 
-  PROCEDURE CheckCase(VAR x: ORG.Item; obj: ORB.Object; sel: BOOLEAN);
+  PROCEDURE CheckCase(VAR x: ORG.Item; obj: ORB.Object);
   BEGIN
-    IF ~sel & (obj.lev > 0) & (obj.exno > 0) & (obj.type.form = ORB.Pointer) THEN x.rdo := TRUE END
+    IF ~(ORG.selected IN x.status) & (obj.lev > 0) & (obj.exno > 0) & (obj.type.form = ORB.Pointer) THEN
+      INCL(x.status, ORG.rdo)
+    END
   END CheckCase;
 
-  PROCEDURE SetSel(sel: BOOLEAN);
-  BEGIN
-    IF sel THEN selected := TRUE END
-  END SetSel;
-
-  PROCEDURE CheckReceiver(proc: ORB.Object; deref: BOOLEAN);
+  PROCEDURE CheckReceiver(VAR x: ORG.Item; proc: ORB.Object);
   BEGIN
-    IF ~deref & (proc.type.dsc.class # ORB.Par) THEN ORS.Mark("incompatible receiver") END
+    IF ~(ORG.deref IN x.status) & (proc.type.dsc.class # ORB.Par) THEN ORS.Mark("incompatible receiver") END
   END CheckReceiver;
 
   PROCEDURE IsExtension(t0, t1: ORB.Type): BOOLEAN;
@@ -142,11 +138,11 @@
     IF ~guard THEN x.type := ORB.boolType END
   END TypeTest;
 
-  PROCEDURE selector(VAR x: ORG.Item; VAR sel: BOOLEAN);
-    VAR y: ORG.Item; obj, fld: ORB.Object; xt: ORB.Type; deref, s: BOOLEAN;
-  BEGIN deref := FALSE; s := FALSE;
+  PROCEDURE selector(VAR x: ORG.Item);
+    VAR y: ORG.Item; obj, fld: ORB.Object; xt: ORB.Type;
+  BEGIN
     WHILE (sym = ORS.lbrak) OR (sym = ORS.period) OR (sym = ORS.arrow)
-        OR (sym = ORS.lparen) & (x.type.form IN {ORB.Record, ORB.Pointer}) DO s := TRUE;
+        OR (sym = ORS.lparen) & (x.type.form IN {ORB.Record, ORB.Pointer}) DO ORG.Select(x, TRUE);
       IF sym = ORS.lbrak THEN
         IF x.type.form = ORB.Pointer THEN ORG.DeRef(x); x.type := x.type.base END ;
         REPEAT ORS.Get(sym); expression(y);
@@ -155,11 +151,11 @@
           ELSE ORS.Mark("not an array")
           END
         UNTIL sym # ORS.comma;
-        Check(ORS.rbrak, "no ]"); deref := FALSE
+        Check(ORS.rbrak, "no ]")
       ELSIF sym = ORS.period THEN
         ORS.Get(sym);
         IF sym = ORS.ident THEN
-          IF x.type.form = ORB.Pointer THEN ORG.DeRef(x); x.type := x.type.base; deref := TRUE END ;
+          IF x.type.form = ORB.Pointer THEN ORG.DeRef(x); x.type := x.type.base END ;
           IF x.type.form = ORB.Record THEN
             obj := ORB.thisfield(x.type); ORS.Get(sym);
             IF obj # NIL THEN
@@ -169,14 +165,14 @@
                   IF fld # NIL THEN xt := xt.base; fld := NIL;
                     WHILE (xt # NIL) & (fld = NIL) DO fld := ORB.FindFld(obj.name, xt); xt := xt.base END ;
                     IF (fld # NIL) & (fld.type.form = ORB.TProc) THEN obj := fld;
-                      CheckReceiver(obj, deref); ORG.Method(x, obj, deref, TRUE); DisallowMethods(x.type.base)
+                      CheckReceiver(x, obj); ORG.Method(x, obj, TRUE); DisallowMethods(x.type.base)
                     ELSE ORS.Mark("method undefined in base types")
                     END
                   ELSE ORS.Mark("method undefined at this extension level")
                   END
-                ELSE (*method call*) CheckReceiver(obj, deref); ORG.Method(x, obj, deref, FALSE)
+                ELSE (*method call*) CheckReceiver(x, obj); ORG.Method(x, obj, FALSE)
                 END
-              ELSE ORG.Field(x, obj); deref := FALSE
+              ELSE ORG.Field(x, obj)
               END ;
               x.type := obj.type
             ELSE ORS.Mark("undef")
@@ -187,7 +183,7 @@
         END
       ELSIF sym = ORS.arrow THEN
         ORS.Get(sym);
-        IF x.type.form = ORB.Pointer THEN ORG.DeRef(x); x.type := x.type.base; deref := TRUE
+        IF x.type.form = ORB.Pointer THEN ORG.DeRef(x); x.type := x.type.base
         ELSE ORS.Mark("not a pointer")
         END
       ELSIF (sym = ORS.lparen) & (x.type.form IN {ORB.Record, ORB.Pointer}) THEN (*type guard*)
@@ -201,8 +197,7 @@
         END ;
         Check(ORS.rparen, " ) missing")
       END
-    END ;
-    sel := s
+    END
   END selector;
 
   PROCEDURE EqualSignatures(t0, t1: ORB.Type): BOOLEAN;
@@ -369,7 +364,7 @@
     IF sym = ORS.ident THEN
       qualident(obj);
       IF obj.class = ORB.SFunc THEN StandFunc(x, obj.val, obj.type)
-      ELSE ORG.MakeItem(x, obj); selector(x, sel); SetSel(sel); CheckCase(x, obj, sel);
+      ELSE ORG.MakeItem(x, obj); selector(x); CheckCase(x, obj);
         IF sym = ORS.lparen THEN
           ORS.Get(sym);
           IF (x.type.form IN {ORB.Proc, ORB.TProc}) & (x.type.base.form # ORB.NoTyp) THEN
@@ -608,9 +603,9 @@
       IF sym = ORS.ident THEN
         qualident(obj); ORG.MakeItem(x, obj);
         IF x.mode = ORB.SProc THEN StandProc(obj.val)
-        ELSE selector(x, sel);
+        ELSE selector(x);
           IF sym = ORS.becomes THEN (*assignment*)
-            ORS.Get(sym); CheckCase(x, obj, sel); CheckReadOnly(x); expression(y);
+            ORS.Get(sym); CheckCase(x, obj); CheckReadOnly(x); expression(y);
             IF CompTypes(x.type, y.type, FALSE) THEN
               IF (x.type.form <= ORB.Pointer) OR (x.type.form = ORB.Proc) THEN ORG.Store(x, y)
               ELSE ORG.StoreStruct(x, y)
@@ -684,8 +679,8 @@
       ELSIF sym = ORS.case THEN
         ORS.Get(sym);
         IF sym = ORS.ident THEN obj := ORB.thisObj() END ;
-        selected := FALSE; expression(x);
-        IF ~selected & (obj # NIL) & (obj.class # ORB.Mod) & (obj.lev > 0) &
+        ORG.Select(x, FALSE); expression(x);
+        IF ~(ORG.selected IN x.status) & (obj # NIL) & (obj.class # ORB.Mod) & (obj.lev > 0) &
           ((x.type.form = ORB.Pointer) & (x.mode = ORB.Var) & (x.type.base.form = ORB.Record) OR
           (x.type.form = ORB.Record) & (x.mode = ORB.Par)) THEN TypeCasePart(obj)
         ELSE
```
