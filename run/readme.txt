Διαδικασία μεταγλώττισης ενός προγράμματος:

1. Μεταγλώττιση χρησιμοποιώντας το μεταγλωττιστή σας
   για να πάρετε το αρχείο:
   
      program.asm

2. Κλήση του συμβολομεταφραστή (assembler):

      masm /Mx program.asm;

3. Κλήση του συνδέτη (linker):

      link /tiny /noignorecase program.obj,program.com,nul,grace.lib;
