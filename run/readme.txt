(*** Encoding: Greek iso-8859-7 ***)

Διαδικασία μεταγλώττισης ενός προγράμματος:

1. Μεταγλώττιση χρησιμοποιώντας το μεταγλωττιστή σας για να πάρετε
   το αρχείο "program.asm".

2. Κλήση του συμβολομεταφραστή (assembler) και του συνδέτη (linker):

      ml /AT /Cx /nologo /Zm program.asm /link lang.lib

   όπου "lang.lib" το όνομα της βιβλιοθήκης χρόνου εκτέλεσης.

   Εναλλακτικά, μπορείτε να κάνετε ξεχωριστά τη συμβολομετάφραση
   και τη σύνδεση:

   2α. Κλήση του συμβολομεταφραστή (assembler):

       masm /Mx /t program.asm;

   2β. Κλήση του συνδέτη (linker):

       link /tiny /noignorecase /nologo program.obj,program.com,nul,lang.lib;
