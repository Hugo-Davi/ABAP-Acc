*&---------------------------------------------------------------------*
*& Include          MZ12AL10_CCOMEX_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*
FORM F_LOG USING P_LOG_STATUS
                 P_LOG_MESSAGE
                .
  CLEAR WA_LOG.
      WA_LOG-nsol = P_NSOL.
      WA_LOG-usuario = SY-uname.
      WA_LOG-STATUS = P_LOG_STATUS.
      WA_LOG-MESSAGE = P_LOG_MESSAGE."'Criação de ordem de venda cancelada.'.
      WA_LOG-data = SY-DATUM.
      WA_LOG-HORA = SY-UZEIT.

      MODIFY ZTBCOMEX_10_LOG FROM WA_LOG.
      IF SY-SUBRC <> 0.
        MESSAGE: 'Erro ao salvar no Banco de Dados' TYPE 'W'.
      ENDIF.
ENDFORM.

FORM F_BAPI_CRIAR.
  DATA: L_MSG_RETURN(300) TYPE C.

  DATA: header LIKE bapisdhead1.
  DATA: headerx LIKE bapisdhead1x.
  DATA: item    LIKE bapisditem OCCURS 0 WITH HEADER LINE.
  DATA: itemx   LIKE bapisditemx OCCURS 0 WITH HEADER LINE.
  DATA: partner LIKE bapipartnr  OCCURS 0 WITH HEADER LINE.
  DATA: return  LIKE bapiret2    OCCURS 0 WITH HEADER LINE.
  DATA: lt_schedules_inx   TYPE STANDARD TABLE OF bapischdlx
                           WITH HEADER LINE.
  DATA: lt_schedules_in    TYPE STANDARD TABLE OF bapischdl
                           WITH HEADER LINE.

*   HEADER DATA
  header-doc_type = p_auart.
  headerx-doc_type = 'X'.

  header-PURCH_NO_C = p_nsol.
  headerx-PURCH_NO_C = 'X'.

  header-sales_org = p_vkorg.
  headerx-sales_org = 'X'.

  header-distr_chan  = p_vtweg.
  headerx-distr_chan = 'X'.

  header-division = p_spart.
  headerx-division = 'X'.

  headerx-updateflag = 'I'.

*   PARTNER DATA
  partner-partn_role = 'AG'.
  partner-partn_numb = p_EMI_KUNNR.
  APPEND partner.

  partner-partn_role = 'WE'.
  partner-partn_numb = p_REC_KUNNR.
  APPEND partner.

  LOOP AT MATE_TC_ITAB INTO MATE_TC_WA.
    CLEAR item.
    CLEAR itemx.
*   ITEM DATA
  itemx-updateflag = 'I'.

  item-itm_number = '000010'.
  itemx-itm_number = 'X'.


  item-material = MATE_TC_WA-matnr.
  itemx-material = 'X'.

  item-plant    = MATE_TC_WA-PRCTR.
  itemx-plant   = 'X'.

  item-target_qty = MATE_TC_WA-KWMENG.
  itemx-target_qty = 'X'.

  item-target_qu = 'PEÇ'.
  itemx-target_qu = 'X'.

  item-item_categ = MATE_TC_WA-PSTYV.
  itemx-item_categ = 'X'.

  APPEND item.
  APPEND itemx.

*     Fill schedule lines
  lt_schedules_in-itm_number = '000010'.
  lt_schedules_in-sched_line = '0001'.
  lt_schedules_in-req_qty    = MATE_TC_WA-KWMENG.
  APPEND lt_schedules_in.

*     Fill schedule line flags
  lt_schedules_inx-itm_number  = '000010'.
  lt_schedules_inx-sched_line  = '0001'.
  lt_schedules_inx-updateflag  = 'X'.
  lt_schedules_inx-req_qty     = 'X'.
  APPEND lt_schedules_inx.

  CLEAR MATE_TC_WA.
ENDLOOP.
*   Call the BAPI
  CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA1'
       EXPORTING
            sales_header_in     = header
            sales_header_inx    = headerx
       IMPORTING
            salesdocument_ex    = V_VBELN
       TABLES
            return              = return
            sales_items_in      = item
            sales_items_inx     = itemx
            sales_schedules_in  = lt_schedules_in
            sales_schedules_inx = lt_schedules_inx
            sales_partners      = partner.

*   Check the return table.
  READ TABLE  return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
  " SE NÃO TEM MENSAGEM DE ERRO NO RETORNO ...
  IF sy-subrc <> 0.
    COMMIT WORK AND WAIT.
    LOOP AT RETURN INTO DATA(ls_return).
      IF ls_return-type = 'S'.
        L_MSG_RETURN = ls_return-MESSAGE. " Procurando mensagem de Sucesso
      ENDIF.
    ENDLOOP.
    PERFORM: F_LOG USING '4'
                         L_MSG_RETURN. " Registrando na tabela de LOG
    CLEAR WA_COMEX.
    WA_COMEX-NSOL = P_NSOL.
    WA_COMEX-VBELN = V_VBELN.
    WA_COMEX-USUARIO = SY-UNAME.
    WA_COMEX-DATA = SY-DATUM.
    MODIFY ZTBCOMEX_10 FROM WA_COMEX. " Adicionando na tabela COMEX10
    COMMIT WORK.
    SET PARAMETER ID 'AUN' FIELD V_VBELN.
    CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
" SE TEM MENSAGEM DE ERRO NO RETORNO ...
  ELSE.
    LOOP AT RETURN INTO DATA(ls_return_err).
      IF ls_return_err-type = 'E'.
        L_MSG_RETURN = ls_return_err-MESSAGE. " Procurando mensagem de Erro
        exit.
      ENDIF.
    ENDLOOP.
    PERFORM: F_LOG USING '5'
                         l_msg_return.  " Registrando na tabela de LOG
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
        TITEL              = 'Erro'
        textline1          = l_msg_return
*       TEXTLINE2          = ' '
*       START_COLUMN       = 25
*       START_ROW          = 6
              .

    LEAVE PROGRAM.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                          P_TABLE_NAME
                          P_MARK_NAME
                 CHANGING P_OK      LIKE SY-UCOMM.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA: L_OK              TYPE SY-UCOMM,
         L_OFFSET          TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
   SEARCH P_OK FOR P_TC_NAME.
   IF SY-SUBRC <> 0.
     EXIT.
   ENDIF.
   L_OFFSET = STRLEN( P_TC_NAME ) + 1.
   L_OK = P_OK+L_OFFSET.
*&SPWIZARD: execute general and TC specific operations                 *
   CASE L_OK.
     WHEN 'INSR'."'INSR'.                      "insert row
       PERFORM FCODE_INSERT_ROW USING    P_TC_NAME
                                         P_TABLE_NAME.
       CLEAR P_OK.

     WHEN 'DELE'.                      "delete row
       PERFORM FCODE_DELETE_ROW USING    P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME.
       CLEAR P_OK.

     WHEN 'P--' OR                     "top of list
          'P-'  OR                     "previous page
          'P+'  OR                     "next page
          'P++'.                       "bottom of list
       PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                             L_OK.
       CLEAR P_OK.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
     WHEN 'MARK'.                      "mark all filled lines
       PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME   .
       CLEAR P_OK.

     WHEN 'DMRK'.                      "demark all filled lines
       PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                           P_TABLE_NAME
                                           P_MARK_NAME .
       CLEAR P_OK.

   ENDCASE.

 ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_insert_row
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_LINES_NAME       LIKE FELD-NAME.
   DATA L_SELLINE          LIKE SY-STEPL.
   DATA L_LASTLINE         TYPE I.
   DATA L_LINE             TYPE I.
   DATA L_TABLE_NAME       LIKE FELD-NAME.
   FIELD-SYMBOLS <TC>                 TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <TABLE>              TYPE STANDARD TABLE.
   FIELD-SYMBOLS <LINES>              TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE P_TC_NAME '_LINES' INTO L_LINES_NAME.
   ASSIGN (L_LINES_NAME) TO <LINES>.

*&SPWIZARD: get current line                                           *
   GET CURSOR LINE L_SELLINE.
   IF SY-SUBRC <> 0.                   " append line to table
     L_SELLINE = <TC>-LINES + 1.
*&SPWIZARD: set top line                                               *
     IF L_SELLINE > <LINES>.
       <TC>-TOP_LINE = L_SELLINE - <LINES> + 1 .
     ELSE.
       <TC>-TOP_LINE = 1.
     ENDIF.
   ELSE.                               " insert line into table
     L_SELLINE = <TC>-TOP_LINE + L_SELLINE - 1.
     L_LASTLINE = <TC>-TOP_LINE + <LINES> - 1.
   ENDIF.
*&SPWIZARD: set new cursor line                                        *
   L_LINE = L_SELLINE - <TC>-TOP_LINE + 1.

*&SPWIZARD: insert initial line                                        *
   INSERT INITIAL LINE INTO <TABLE> INDEX L_SELLINE.
   <TC>-LINES = <TC>-LINES + 1.
*&SPWIZARD: set cursor                                                 *
   SET CURSOR 1 L_LINE.

 ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_delete_row
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME
                        P_MARK_NAME   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TABLE_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE cxtab_control.
   FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <WA>.
   FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
   DESCRIBE TABLE <TABLE> LINES <TC>-LINES.

   LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     IF <MARK_FIELD> = 'X'.
       DELETE <TABLE> INDEX SYST-TABIX.
       IF SY-SUBRC = 0.
         <TC>-LINES = <TC>-LINES - 1.
       ENDIF.
     ENDIF.
   ENDLOOP.

 ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME
                                       P_OK.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TC_NEW_TOP_LINE     TYPE I.
   DATA L_TC_NAME             LIKE FELD-NAME.
   DATA L_TC_LINES_NAME       LIKE FELD-NAME.
   DATA L_TC_FIELD_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE cxtab_control.
   FIELD-SYMBOLS <LINES>      TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.
*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
   ASSIGN (L_TC_LINES_NAME) TO <LINES>.


*&SPWIZARD: is no line filled?                                         *
   IF <TC>-LINES = 0.
*&SPWIZARD: yes, ...                                                   *
     L_TC_NEW_TOP_LINE = 1.
   ELSE.
*&SPWIZARD: no, ...                                                    *
     CALL FUNCTION 'SCROLLING_IN_TABLE'
          EXPORTING
               ENTRY_ACT             = <TC>-TOP_LINE
               ENTRY_FROM            = 1
               ENTRY_TO              = <TC>-LINES
               LAST_PAGE_FULL        = 'X'
               LOOPS                 = <LINES>
               OK_CODE               = P_OK
               OVERLAPPING           = 'X'
          IMPORTING
               ENTRY_NEW             = L_TC_NEW_TOP_LINE
          EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
               OTHERS                = 0.
   ENDIF.

*&SPWIZARD: get actual tc and column                                   *
   GET CURSOR FIELD L_TC_FIELD_NAME
              AREA  L_TC_NAME.

   IF SYST-SUBRC = 0.
     IF L_TC_NAME = P_TC_NAME.
*&SPWIZARD: et actual column                                           *
       SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
     ENDIF.
   ENDIF.

*&SPWIZARD: set the new top line                                       *
   <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.


 ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_MARK_LINES USING P_TC_NAME
                               P_TABLE_NAME
                               P_MARK_NAME.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                 P_TABLE_NAME
                                 P_MARK_NAME .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = SPACE.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines