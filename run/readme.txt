(*** Encoding: Greek iso-8859-7 ***)

���������� ������������� ���� ������������:

1. ������������ ��������������� �� ������������� ��� ��� �� ������
   �� ������ "program.asm".

2. ����� ��� ����������������� (assembler) ��� ��� ������� (linker):

      ml /AT /Cx /nologo /Zm program.asm /link lang.lib

   ���� "lang.lib" �� ����� ��� ����������� ������ ���������.

   �����������, �������� �� ������ ��������� �� ����������������
   ��� �� �������:

   2�. ����� ��� ����������������� (assembler):

       masm /Mx /t program.asm;

   2�. ����� ��� ������� (linker):

       link /tiny /noignorecase /nologo program.obj,program.com,nul,lang.lib;
