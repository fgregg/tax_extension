00001 *----------------------------------------------------------------*
00002 *              *  A L L   P R O P - I N F O   S S A ' S  *
00003 *                      *    FOR NEW DATA-BASE   *
00004 *                      *------------------------*
00005  01  PROP-INFO-SSA.
00006 *
00007      05  QUAL-ROOT-SSA.
00008          10  ROOT-SEGNAME  PIC X(08)  VALUE 'PROPSEG '.
00009          10  FILLER        PIC X      VALUE '('.
00010          10  ROOT-SEGKEY   PIC X(08)  VALUE 'PPROP   '.
00011          10  QR-REL-OPER   PIC XX     VALUE 'EQ'.
00012          10  ROOT-SSA-KEY.
00013              15  QR-PROP   PIC 9(15)  COMP-3.
00014          10  FILLER        PIC X      VALUE ')'.
00015 *
00016      05  UNQUAL-ROOT-SSA.
00017          10  FILLER        PIC X(9)   VALUE 'PROPSEG '.
00018 *
00019      05  QUAL-ALT-ROOT-SSA.
00020          10  FILLER          PIC X(08) VALUE 'PROPSEG '.
00021          10  FILLER          PIC X     VALUE '('.
00022          10  ALT-ROOT-SEGKEY PIC X(08) VALUE 'XSRCH   '.
00023          10  QAR-REL-OPER  PIC XX     VALUE 'EQ'.
00024          10  ALT-ROOT-SSA-KEY.
00025              15  QAR-TOWN  PIC 9(3)   COMP-3.
00026              15  QAR-VOL   PIC 9(3)   COMP-3.
00027              15  QAR-PROP  PIC 9(15)  COMP-3.
00028          10  FILLER        PIC X      VALUE ')'.
00029 *
00030 *
00031 *                            **  LEVEL = 2, PARENT = 'PROPSEG'  **
00032 *                                ----------------------------
00033      05  QUAL-NAME-SSA.
00034          10  NAME-SEGNAME  PIC X(08)  VALUE 'NAMESEG '.
00035          10  FILLER        PIC X      VALUE '('.
00036          10  NAME-SEGKEY   PIC X(08)  VALUE 'PBYEAR  '.
00037          10  QN-REL-OPER   PIC XX     VALUE 'EQ'.
00038          10  NAME-SSA-KEY.
00039              15  QN-YR     PIC 99.
00040          10  FILLER        PIC X      VALUE ')'.
00041 *
00042      05  UNQUAL-NAME-SSA.
00043          10  FILLER        PIC X(9)   VALUE 'NAMESEG  '.
00044 *
00045      05  QUAL-ASMT-SSA.
00046          10  ASMT-SEGNAME  PIC X(08)  VALUE 'ASSMTSEG'.
00047          10  FILLER        PIC X      VALUE '('.
00048          10  ASMT-SEGKEY   PIC X(08)  VALUE 'PYSRCH  '.
00049          10  QA-REL-OPER   PIC XX     VALUE 'EQ'.
00050          10  ASSMT-SSA-KEY.
00051              15  QA-PROCYR PIC 99.
00052              15  QA-TXYR   PIC 99.
00053              15  QA-TXTYP  PIC 9.
00054          10  FILLER        PIC X      VALUE ')'.
00055 *
00056      05  UNQUAL-ASMT-SSA.
00057          10  FILLER        PIC X(9)   VALUE 'ASSMTSEG '.
00058 *
00059      05  QUAL-PERMT-SSA.
00060          10  PERMT-SEGNAME PIC X(08)  VALUE 'PERMTSEG'.
00061          10  FILLER        PIC X      VALUE '('.
00062          10  PERMT-SEGKEY  PIC X(08)  VALUE 'PUSRCH  '.
00063          10  QP-REL-OPER   PIC XX     VALUE 'EQ'.
00064          10  PERMT-SSA-KEY.
00065              15  QP-YR     PIC 99.
00066              15  QP-SEQ    PIC 9(5)   COMP-3.
00067          10  FILLER        PIC X      VALUE ')'.
00068 *
00069      05  UNQUAL-PERMT-SSA.
00070          10  FILLER        PIC X(9)   VALUE 'PERMTSEG '.
00071 *
00072      05  QUAL-PERMT-SSA1.
00073          10  FILLER        PIC X(08)  VALUE 'PERMTSEG'.
00074          10  FILLER        PIC X      VALUE '('.
00075          10  PERMT-SEGKEY1 PIC X(08)  VALUE 'PUYR    '.
00076          10  QP-REL-OPER1  PIC XX     VALUE 'EQ'.
00077          10  PERMT-SSA-KEY1.
00078              15  QP-YR1    PIC 99.
00079          10  FILLER        PIC X      VALUE ')'.
00080 *
00081      05  UNQUAL-LEGAL-SSA.
00082          10  LEGAL-SEGKEY  PIC X(8)   VALUE 'LEGALSEG'.
00083          10  FILLER        PIC X      VALUE ' '.
00084 *
00085      05  QUAL-LOCA-SSA.
00086          10  LOCAT-SEGNAME PIC X(08)  VALUE 'LOCATSEG'.
00087          10  FILLER        PIC X      VALUE '('.
00088          10  LOCAT-SEGKEY  PIC X(08)  VALUE 'LCSEGKEY'.
00089          10  QL-REL-OPER   PIC XX     VALUE 'EQ'.
00090          10  LOCAT-SSA-KEY.
00091              15  QL-TOWNNO PIC XX.
00092              15  QL-CITYCD PIC X(5).
00093              15  QL-STRTCD PIC X(5).
00094          10  FILLER        PIC X      VALUE ')'.
00095 *
00096      05  UNQUAL-LOCA-SSA.
00097          10  FILLER        PIC X(9)   VALUE 'LOCATSEG '.
00098 *
00099 *
00100 *
00101 *
00102 *                            **  LEVEL = 3, PARENT = 'ASSMTSEG' **
00103 *
00104      05  QUAL-CMPLNT-SSA.
00105          10  CMPLNT-SEGNAME PIC X(08) VALUE 'CMPLTSEG'.
00106          10  FILLER         PIC X     VALUE '('.
00107          10  CMPLNT-SEGKEY  PIC X(08) VALUE 'PGTYPE  '.
00108          10  QC-REL-OPER   PIC XX     VALUE 'EQ'.
00109          10  CMPLNT-SSA-KEY.
00110              15  QC-TYPE   PIC X.
00111          10  FILLER        PIC X      VALUE ')'.
00112 *
00113      05  UNQUAL-CMPLNT-SSA.
00114          10  FILLER        PIC X(9)   VALUE 'CMPLTSEG '.
00115 *
00116      05  QUAL-QUEST-SSA.
00117          10  QUEST-SEGNAME PIC X(08)  VALUE 'QUESTSEG'.
00118          10  FILLER        PIC X      VALUE '('.
00119          10  QUEST-SEGKEY  PIC X(08)  VALUE 'PQKEY   '.
00120          10  QQ-REL-OPER   PIC XX     VALUE 'EQ'.
00121          10  QUEST-SSA-KEY.
00122              15  QQ-PASS   PIC 9.
00123              15  QQ-MC     PIC 9(3)   COMP-3.
00124              15  QQ-SEGCD  PIC X.
00125          10  FILLER        PIC X      VALUE ')'.
00126 *
00127      05  UNQUAL-QUEST-SSA.
00128          10  FILLER        PIC X(9)   VALUE 'QUESTSEG '.
00129 *
00130      05  QUAL-EXEMPT-SSA.
00131          10  EXEMPT-SEGNAME PIC X(08) VALUE 'EXMPTSEG'.
00132          10  FILLER         PIC X     VALUE '('.
00133          10  EXEMPT-SEGKEY  PIC X(08) VALUE 'PESEGCOD'.
00134          10  QE-REL-OPER   PIC XX     VALUE 'EQ'.
00135          10  EXEMPT-SSA-KEY.
00136              15  QE-SEGCD  PIC X.
00137          10  FILLER        PIC X      VALUE ')'.
00138 *
00139      05  UNQUAL-EXEMPT-SSA.
00140          10  FILLER        PIC X(9)   VALUE 'EXMPTSEG '.
00141 *
00142      05  QUAL-PAY-REF-SSA.
00143          10  PAYREF-SEGNAME PIC X(08) VALUE 'PAYMTSEG'.
00144          10  FILLER         PIC X     VALUE '('.
00145          10  PAYREF-SEGKEY  PIC X(08) VALUE 'PPKEY   '.
00146          10  QPR-REL-OPER  PIC XX     VALUE 'EQ'.
00147          10  PAY-REF-SSA-KEY.
00148              15  QPR-TYPE  PIC X.
00149          10  FILLER        PIC X      VALUE ')'.
00150 *
00151      05  UNQUAL-PAY-REF-SSA.
00152          10  FILLER        PIC X(9)   VALUE 'PAYMTSEG '.
00153 *
00154      05  QUAL-MISC-SSA.
00155          10  MISC-SEGNAME  PIC X(08)  VALUE 'MISCSEG '.
00156          10  FILLER        PIC X      VALUE '('.
00157          10  MISC-SEGKEY   PIC X(08)  VALUE 'PMSEGCOD'.
00158          10  QM-REL-OPER   PIC XX     VALUE 'EQ'.
00159          10  MISC-SSA-KEY.
00160              15  QM-SEGCD  PIC X.
00161          10  FILLER        PIC X      VALUE ')'.
00162 *
00163      05  UNQUAL-MISC-SSA.
00164          10  FILLER        PIC X(9)   VALUE 'MISCSEG  '.
00165 *
00166      05  QUAL-SALES-SSA.
00167          10  SALES-SEGNAME PIC X(08)  VALUE 'SALESSEG'.
00168          10  FILLER        PIC X      VALUE '('.
00169          10  SALES-SEGKEY  PIC X(08)  VALUE 'PZDATE  '.
00170          10  QS-REL-OPER   PIC XX     VALUE 'EQ'.
00171          10  SALES-SSA-KEY.
00172              15  QS-DATE   PIC 9(7)   COMP-3.
00173          10  FILLER        PIC X      VALUE ')'.
00174 *
00175      05  UNQUAL-SALES-SSA.
00176          10  FILLER        PIC X(9)   VALUE 'SALESSEG '.
00177 *
00178 *
00179 *                            **  LEVEL = 4, PARENT = 'EXMPTSEG' **
00180 *
00181      05  QUAL-HOMECOOP-SSA.
00182          10  HOMECOOP-SEGNAME PIC X(08) VALUE 'COOPSEG '.
00183          10  FILLER           PIC X     VALUE '('.
00184          10  HOMECOOP-SEGKEY  PIC X(08) VALUE 'PHBTHDTE'.
00185          10  QH-REL-OPER   PIC XX     VALUE 'EQ'.
00186          10  HOMECOOP-SSA-KEY.
00187              15  QH-BTHDTE PIC 9(7)   COMP-3.
00188          10  FILLER        PIC X      VALUE ')'.
00189 *
00190      05  UNQUAL-HOMECOOP-SSA.
00191          10  FILLER        PIC X(9)   VALUE 'COOPSEG  '.
00192 *
00193 *
00194 *
00195 *                            **  SSA'S USED WITH A COMMAND CODE **
00196 *                                -----------------------------
00197      05  UNQUAL-CMDCDE-SSA.
00198          10  UCC-SEGNAME   PIC X(8)   VALUE SPACES.
00199          10  FILLER        PIC X      VALUE '*'.
00200          10  UCC-CMDCDE    PIC X      VALUE SPACE.
00201 *                                                                *
00202 *                      DEFINE A 'SSA-KEY' AFTER 'QCC-REL-OPER',  *
00203 *                         FOLLOWED WITH A FILLER -  VALUE ')'    *
00204      05  QUAL-CMDCDE-SSA.
00205          10  QCC-SEGNAME   PIC X(8)   VALUE SPACES.
00206          10  FILLER        PIC X      VALUE '*'.
00207          10  QCC-CMDCDE    PIC X      VALUE SPACE.
00208          10  FILLER        PIC X      VALUE '('.
00209          10  QCC-FLDNAME   PIC X(8)   VALUE SPACES.
00210          10  QCC-REL-OPER  PIC XX     VALUE 'EQ'.
00211 *----------------------------------------------------------------*
