00001 **************************************************************
00002 *             ASSESSOR ASSESSMENT INFORMATION                *
00003 *                       DATA BASE                            *
00004 *             PATH TO: 1) ROOT SEGMENT                       *
00005 *                      2) ASSESSMENT/TAX INFORMATION SEGMENT *
00006 *                      3) EXEMPTION MASTER SEGMENT           *
00007 *                      4) EXEMPTION DETAIL SEGMENT           *
00008 *                      5) SOCIAL SECURITY SEGMENT            *
00009 **************************************************************
00010 *
00011  01  SSAS-29.
00012 *--------------------- QUALIFIED SSA'S ----------------------*
00013      05  LVL1-QUAL-SSA.
00014          10  LVL1-SEGNAME        PIC X(8)   VALUE 'PROPSEG '.
00015          10  FILLER              PIC X      VALUE '('.
00016          10  FILLER              PIC X(8)   VALUE 'PPROP   '.
00017          10  LVL1-RO             PIC XX     VALUE 'EQ'.
00018          10  LVL1-ARG            PIC 9(15)    COMP-3.
00019          10  FILLER              PIC X      VALUE ')'.
00020      05  LVL2-QUAL-SSA.
00021          10  LVL2-SEGNAME        PIC X(8)   VALUE 'ASSMTSEG'.
00022          10  FILLER              PIC X      VALUE '('.
00023          10  FILLER              PIC X(8)   VALUE 'PYSRCH  '.
00024          10  LVL2-RO             PIC XX     VALUE 'EQ'.
00025          10  LVL2-ARG.
00026              15  LVL2-PROCYR     PIC 99.
00027              15  LVL2-TXYR       PIC 99.
00028              15  LVL2-TXTYP      PIC 9.
00029          10  FILLER              PIC X      VALUE ')'.
00030      05  LVL3-QUAL-SSA.
00031          10  LVL3-SEGNAME        PIC X(8)   VALUE 'EXMASTSG'.
00032          10  FILLER              PIC X      VALUE '('.
00033          10  FILLER              PIC X(8)   VALUE 'EXRECCOD'.
00034          10  LVL3-RO             PIC XX     VALUE 'EQ'.
00035          10  LVL3-ARG            PIC X.
00036          10  FILLER              PIC X      VALUE ')'.
00037      05  LVL4-QUAL-SSA.
00038          10  LVL4-SEGNAME        PIC X(8)   VALUE 'EXDETLSG'.
00039          10  FILLER              PIC X      VALUE '('.
00040          10  FILLER              PIC X(8)   VALUE 'EXBRTHDT'.
00041          10  LVL4-RO             PIC XX     VALUE 'EQ'.
00042          10  LVL4-ARG            PIC X(8).
00043          10  FILLER              PIC X      VALUE ')'.
00044      05  LVL5-QUAL-SSA.
00045          10  LVL5-SEGNAME        PIC X(8)   VALUE 'EXSOSCSG'.
00046          10  FILLER              PIC X      VALUE '('.
00047          10  FILLER              PIC X(8)   VALUE 'EXSEQNUM'.
00048          10  LVL5-RO             PIC XX     VALUE 'EQ'.
00049          10  LVL5-ARG            PIC XX.
00050          10  FILLER              PIC X      VALUE ')'.
00051 *-------------------- UNQUALIFIED SSA'S ---------------------*
00052      05  LVL1-UNQUAL-SSA         PIC X(9)   VALUE 'PROPSEG'.
00053      05  LVL2-UNQUAL-SSA         PIC X(9)   VALUE 'ASSMTSEG'.
00054      05  LVL3-UNQUAL-SSA         PIC X(9)   VALUE 'EXMASTSG'.
00055      05  LVL4-UNQUAL-SSA         PIC X(9)   VALUE 'EXDETLSG'.
00056      05  LVL5-UNQUAL-SSA         PIC X(9)   VALUE 'EXSOSCSG'.
00057 *-------------------- PATH  SSA'S   -------------------------*
00058      05  LVL1-QUAL-SSA-PATH.
00059          10  FILLER              PIC X(10)  VALUE 'PROPSEG *D'.
00060          10  FILLER              PIC X      VALUE '('.
00061          10  FILLER              PIC X(8)   VALUE 'PPROP   '.
00062          10  LVL1-RO-P           PIC XX     VALUE 'EQ'.
00063          10  LVL1-ARG-P          PIC 9(15)    COMP-3.
00064          10  FILLER              PIC X      VALUE ')'.
00065      05  LVL2-QUAL-SSA-PATH.
00066          10  FILLER              PIC X(10)  VALUE 'ASSMTSEG*D'.
00067          10  FILLER              PIC X      VALUE '('.
00068          10  FILLER              PIC X(8)   VALUE 'PYSRCH  '.
00069          10  LVL2-RO-P           PIC XX     VALUE 'EQ'.
00070          10  LVL2-ARG-P.
00071              15  LVL2-PROCYR-P   PIC 99.
00072              15  LVL2-TXYR-P     PIC 99.
00073              15  LVL2-TXTYP-P    PIC 9.
00074      05  LVL3-QUAL-SSA-PATH.
00075          10  FILLER              PIC X(10)  VALUE 'EXMASTSG*D'.
00076          10  FILLER              PIC X      VALUE '('.
00077          10  FILLER              PIC X(8)   VALUE 'EXRECCOD'.
00078          10  LVL3-RO-P           PIC XX     VALUE 'EQ'.
00079          10  LVL3-ARG-P          PIC X.
00080          10  FILLER              PIC X      VALUE ')'.
00081      05  LVL4-QUAL-SSA-PATH.
00082          10  FILLER              PIC X(10)  VALUE 'EXDETLSG*D'.
00083          10  FILLER              PIC X      VALUE '('.
00084          10  FILLER              PIC X(8)   VALUE 'EXBRTHDT'.
00085          10  LVL4-RO-P           PIC XX     VALUE 'EQ'.
00086          10  LVL4-ARG-P          PIC X(8).
00087          10  FILLER              PIC X      VALUE ')'.
