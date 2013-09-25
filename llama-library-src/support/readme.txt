Διαδικασία μεταγλώττισης ενός προγράμματος Llama:

1. Μεταγλώττιση χρησιμοποιώντας το μεταγλωττιστή σας
   για να πάρετε το αρχείο:
   
      program.asm

2. Κλήση του συμβολομεταφραστή (assembler):

      masm /Mx program.asm;

3. Κλήση του συνδέτη (linker):

      link /tiny /noignorecase program.obj,program.com,nul,llama.lib;
