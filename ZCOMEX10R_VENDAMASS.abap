*&---------------------------------------------------------------------*
*& Report ZCOMEX10R_VENDAMASS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcomex10r_vendamass.
DATA: V_INDEX TYPE I, " Index para controlar os dados
      V_DADOS_PRONTOS TYPE ABAP_BOOL, " Identificar se os dados já estão preparados para fazer uma ordem de venda
      V_NSOL TYPE ZTBCOMEX_10.

DATA: wa_layout   TYPE slis_layout_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      it_fieldcat TYPE slis_t_fieldcat_alv.
* TABELA PARA TRAATAR O ARQUIVO
TYPES: BEGIN OF ty_tab,
         line TYPE string,
       END OF ty_tab.
DATA: it_tab TYPE STANDARD TABLE OF ty_tab,
      wa_tab TYPE ty_tab.
* TABELA PARA OS DADOS DO ALV
TYPES: BEGIN OF TY_ALV,
  SEMAFORO TYPE C LENGTH 4,
  NSOL TYPE ZTBCOMEX_10-NSOL,
  VBELN TYPE ZTBCOMEX_10-VBELN,
  VKORG TYPE VBAK-VKORG,
  MATNR TYPE VBAP-MATNR,
  KWMENG TYPE VBAP-KWMENG,
  MESSAGE TYPE ZTBCOMEX_10_LOG,
  END OF TY_ALV.
DATA: IT_ALV TYPE STANDARD TABLE OF TY_ALV,
      WA_ALV TYPE TY_ALV.
* TABELA PARA O CABEÇALHO DAS ORDENS DE VENDA
TYPES: BEGIN OF TY_HEAD,
  IDT TYPE C LENGTH 1,
  AUART LIKE VBAK-AUART,
  VKORG LIKE VBAK-VKORG,
  VTWEG LIKE VBAK-VTWEG,
  SPART LIKE VBAK-SPART,
  SOLD_TO LIKE KUAGV-KUNNR,
  SHIP_TO LIKE KUWEV-KUNNR,
  END OF TY_HEAD.
DATA: IT_HEAD TYPE STANDARD TABLE OF TY_HEAD,
      WA_HEAD TYPE TY_HEAD.
* TABELA PARA OS MATERIAIS DE CADA ORDEM DE VENDA
TYPES: BEGIN OF TY_MATE,
  IDT TYPE C LENGTH 2,
  MATNR LIKE VBAP-MATNR,
  KWMENG LIKE VBAP-KWMENG,
  WERKS LIKE VBAP-WERKS,
  PSTYV LIKE VBAP-PSTYV,
  END OF TY_MATE.
DATA: IT_MATE TYPE STANDARD TABLE OF TY_MATE,
      WA_MATE TYPE TY_MATE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
  PARAMETERS p_file TYPE localfile. "paramatro para ler arquivo
SELECTION-SCREEN END OF BLOCK b1.
DATA v_file TYPE string.

*&-------------------------------------&
*&-------------  INICIO  --------------&
*&------------  DA LÓGICA -------------&
*&-------------------------------------&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
        IMPORTING
          file_name = p_file.
    IF SY-SUBRC <> 0.
    ELSE.
    v_file = p_file.
    CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename            = V_file
          has_field_separator = ';'
        TABLES
          data_tab            = it_tab
        .
     IF sy-subrc <> 0.
     ENDIF.
   ENDIF.
END-OF-SELECTION.
  LOOP AT IT_TAB INTO WA_TAB.
    IF WA_TAB-LINE+0(1) CP 'H'.
      CLEAR WA_HEAD.
      IF V_DADOS_PRONTOS IS INITIAL.
        V_DADOS_PRONTOS = ABAP_TRUE.
      ELSEIF V_DADOS_PRONTOS = ABAP_TRUE.
        PERFORM F_BAPI.
      ENDIF.
      V_INDEX = 1.
      SPLIT WA_TAB-LINE AT ' ; ' INTO TABLE DATA(LIT_PARTS_HEAD).
      LOOP AT LIT_PARTS_HEAD INTO DATA(LV_VALUE_HEAD).
         CASE V_INDEX.
         	WHEN 1.
            WA_HEAD-IDT = LV_VALUE_HEAD.
         	WHEN 2.
            WA_HEAD-AUART = LV_VALUE_HEAD.
         	WHEN 3.
            WA_HEAD-VKORG = LV_VALUE_HEAD.
          WHEN 4.
            WA_HEAD-VTWEG = LV_VALUE_HEAD.
          WHEN 5.
            WA_HEAD-SPART = LV_VALUE_HEAD.
          WHEN 6.
            WA_HEAD-SOLD_TO = LV_VALUE_HEAD.
            IF STRLEN( WA_HEAD-SOLD_TO ) < 10.
              DATA(lv_zeros_to_add1) = 10 - STRLEN( WA_HEAD-SOLD_TO ).
              DO LV_ZEROS_TO_ADD1 TIMES.
                WA_HEAD-SOLD_TO = '0' && WA_HEAD-SOLD_TO.
              ENDDO.
            ENDIF.
          WHEN 7.
            WA_HEAD-SHIP_TO = LV_VALUE_HEAD.
            IF STRLEN( WA_HEAD-SHIP_TO ) < 10.
              DATA(lv_zeros_to_add2) = 10 - STRLEN( WA_HEAD-SHIP_TO ).
              DO LV_ZEROS_TO_ADD2 TIMES.
                WA_HEAD-SHIP_TO = '0' && WA_HEAD-SHIP_TO.
              ENDDO.
            ENDIF.
         ENDCASE.
         V_INDEX = V_INDEX + 1.
      ENDLOOP.
      APPEND WA_HEAD TO IT_HEAD.
    ELSEIF WA_TAB-LINE+0(1) CP 'I'.
      CLEAR WA_MATE.
      V_INDEX = 1.
      SPLIT WA_TAB-LINE AT ' ; ' INTO TABLE DATA(LIT_PARTS_MATE).
      LOOP AT LIT_PARTS_MATE INTO DATA(LV_VALUE_MATE).
         CASE V_INDEX.
         	WHEN 1.
            WA_MATE-IDT = LV_VALUE_MATE.
         	WHEN 2.
            WA_MATE-MATNR = LV_VALUE_MATE.
            IF STRLEN( WA_MATE-MATNR ) < 18.
              DATA(lv_zeros_to_add) = 18 - STRLEN( WA_MATE-MATNR ).
              DO LV_ZEROS_TO_ADD TIMES.
                WA_MATE-MATNR = '0' && WA_MATE-MATNR.
              ENDDO.
            ENDIF.
         	WHEN 3.
            WA_MATE-KWMENG = LV_VALUE_MATE.
          WHEN 4.
            WA_MATE-WERKS = LV_VALUE_MATE.
          WHEN 5.
            WA_MATE-PSTYV = LV_VALUE_MATE.
         ENDCASE.
         V_INDEX = V_INDEX + 1.
      ENDLOOP.
      APPEND WA_MATE TO IT_MATE.
    ENDIF.
  ENDLOOP.
  PERFORM f_bapi.
  PERFORM F_DISPLAY_ALV.


FORM F_BAPI.
  PERFORM: F_NSOL.
  PERFORM: F_LOG_MASS USING '2'
                            'Criação de ordem de venda de forma automática iniciada'.
  DATA: WA_COMEX TYPE ZTBCOMEX_10.

  DATA: L_MSG_RETURN(300) TYPE C.
  DATA: L_VBELN TYPE VBAP-VBELN.
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

  READ TABLE IT_HEAD INDEX 1 INTO WA_HEAD.
*   HEADER DATA
  header-doc_type = WA_HEAD-auart.
  headerx-doc_type = 'X'.

  header-PURCH_NO_C = v_nsol.
  headerx-PURCH_NO_C = 'X'.

  header-sales_org = WA_HEAD-Vkorg.
  headerx-sales_org = 'X'.

  header-distr_chan  = WA_HEAD-vtweg.
  headerx-distr_chan = 'X'.

  header-division = WA_HEAD-spart.
  headerx-division = 'X'.

  headerx-updateflag = 'I'.

*   PARTNER DATA
  partner-partn_role = 'AG'.
  partner-partn_numb = WA_HEAD-SOLD_TO.
  APPEND partner.

  partner-partn_role = 'WE'.
  partner-partn_numb = WA_HEAD-SHIP_TO.
  APPEND partner.

  LOOP AT IT_MATE INTO WA_MATE.
    CLEAR item.
    CLEAR itemx.

*      CLEAR WA_MATE.
*   ITEM DATA
  itemx-updateflag = 'I'.

  item-itm_number = '000010'.
  itemx-itm_number = 'X'.


  item-material = WA_MATE-matnr.
  itemx-material = 'X'.

  item-plant    = WA_MATE-WERKS.
  itemx-plant   = 'X'.

  item-target_qty = WA_MATE-KWMENG.
  itemx-target_qty = 'X'.

  item-target_qu = 'PEÇ'.
  itemx-target_qu = 'X'.

  item-item_categ = WA_MATE-PSTYV.
  itemx-item_categ = 'X'.

  APPEND item.
  APPEND itemx.

*     Fill schedule lines
  lt_schedules_in-itm_number = '000010'.
  lt_schedules_in-sched_line = '0001'.
  lt_schedules_in-req_qty    = WA_MATE-KWMENG.
  APPEND lt_schedules_in.

*     Fill schedule line flags
  lt_schedules_inx-itm_number  = '000010'.
  lt_schedules_inx-sched_line  = '0001'.
  lt_schedules_inx-updateflag  = 'X'.
  lt_schedules_inx-req_qty     = 'X'.
  APPEND lt_schedules_inx.


ENDLOOP.
*   Call the BAPI
  CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA1'
       EXPORTING
            sales_header_in     = header
            sales_header_inx    = headerx
       IMPORTING
            salesdocument_ex    = L_VBELN
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
        L_MSG_RETURN = ls_return-MESSAGE. " Procurando texto de SUCESSO
      ENDIF.
    ENDLOOP.
    PERFORM: F_LOG_MASS USING '4' " Salvando na tabela de LOG
                         L_MSG_RETURN.
    CLEAR WA_COMEX.
    WA_COMEX-NSOL = V_NSOL.
    WA_COMEX-VBELN = L_VBELN.
    WA_COMEX-USUARIO = SY-UNAME.
    WA_COMEX-DATA = SY-DATUM.
    MODIFY ZTBCOMEX_10 FROM WA_COMEX. " Salvando na tabela COMEX10
    COMMIT WORK.

    PERFORM F_ADD_TO_ALV USING L_MSG_RETURN " Adicionando na tabela interna da ALV
                               '4'
                               V_NSOL
                               L_VBELN
                               WA_HEAD-Vkorg
                               WA_MATE-MATNR
                               WA_MATE-KWMENG.

    CLEAR IT_HEAD.
    CLEAR IT_MATE.
      " SE TEM MENSAGEM DE ERRO NO RETORNO ...
  ELSE.
    LOOP AT RETURN INTO DATA(ls_return_err).
      IF ls_return_err-type = 'E'.
        L_MSG_RETURN = ls_return_err-MESSAGE. " Procurando texto de ERRO
        exit.
      ENDIF.
    ENDLOOP.
    PERFORM: F_LOG_MASS USING '5' " Salvando na tabela de LOG
                         l_msg_return.
    PERFORM F_ADD_TO_ALV USING L_MSG_RETURN " Adicionando na tabela interna da ALV
                               '5'
                               V_NSOL
                               L_VBELN
                               WA_HEAD-Vkorg
                               WA_MATE-MATNR
                               WA_MATE-KWMENG.
    CLEAR IT_HEAD.
    CLEAR IT_MATE.
*    LEAVE PROGRAM.
  ENDIF.
ENDFORM.


FORM F_LOG_MASS USING P_LOG_STATUS
                 P_LOG_MESSAGE
                .
  DATA: WA_LOG TYPE ZTBCOMEX_10_LOG.
  CLEAR WA_LOG.
      WA_LOG-nsol = V_NSOL.
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

FORM F_NSOL.
    call function 'NUMBER_GET_NEXT'
    exporting
      NR_RANGE_NR                   = '1'
      OBJECT                        = 'ZCOMEX10'
      quantity                      = '0000000001'
*     subobject                     = ' '
*     toyear                        = '0000'
*     ignore_buffer                 = ' '
    importing
      number                        = V_nsol
*     quantity                      =
*     returncode                    =
     exceptions
       interval_not_found            = 1
       number_range_not_intern       = 2
       object_not_found              = 3
       quantity_is_0                 = 4
       quantity_is_not_1             = 5
       interval_overflow             = 6
       buffer_overflow               = 7
       others                        = 8
            .
ENDFORM.

FORM F_ADD_TO_ALV USING P_MSG
                        P_STATUS
                        P_NSOL
                        P_VBELN
                        P_VKORG
                        P_MATNR
                        P_KWMENG.
  CLEAR WA_ALV.
  CASE P_STATUS.
    WHEN '1'.
      wa_ALV-SEMAFORO = '@09@'.
    WHEN '2'.
     wa_ALV-SEMAFORO = '@09@'.
    WHEN '3'.
      wa_ALV-SEMAFORO = '@08@'.
    WHEN '4'.
      wa_ALV-SEMAFORO = '@08@'.
    WHEN '5'.
      wa_ALV-SEMAFORO = '@0A@'.
  ENDCASE.
  WA_ALV-NSOL = P_NSOL.
  WA_ALV-VBELN = P_VBELN.
  WA_ALV-VKORG = P_VKORG.
  WA_ALV-MATNR = P_MATNR.
  WA_ALV-KWMENG = P_KWMENG.
  WA_ALV-MESSAGE = P_MSG.
  APPEND WA_ALV TO IT_ALV.
ENDFORM.

FORM F_CAMPOS .
*Exibição de titulos, campos e conteudos da tabela.

  WA_FIELDCAT-FIELDNAME = 'SEMAFORO'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'Semaforo'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'NSOL'.
  WA_FIELDCAT-TABNAME   = 'IT_ALV'.
  WA_FIELDCAT-SELTEXT_S = 'N° da Solicitação'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'VBELN'.
  WA_FIELDCAT-TABNAME   = 'IT_ALV'.
  WA_FIELDCAT-SELTEXT_M = 'N° Ordem de Venda'.
  WA_FIELDCAT-HOTSPOT = ABAP_TRUE.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'VKORG'.
  WA_FIELDCAT-TABNAME   = 'IT_ALV'.
  WA_FIELDCAT-SELTEXT_M = 'Org. Vendas'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'MATNR'.
  WA_FIELDCAT-TABNAME   = 'IT_ALV'.
  WA_FIELDCAT-SELTEXT_M = 'Material'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'KWMENG'.
  WA_FIELDCAT-TABNAME   = 'IT_ALV'.
  WA_FIELDCAT-SELTEXT_M = 'Quant'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'MESSAGE'.
  WA_FIELDCAT-TABNAME   = 'IT_ALV'.
  WA_FIELDCAT-SELTEXT_M = 'Mensagem'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

ENDFORM.

FORM F_DISPLAY_ALV.

  PERFORM F_CAMPOS.
* Layout do ALV
  WA_LAYOUT-ZEBRA = 'X'.
  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = SY-REPID
        I_CALLBACK_USER_COMMAND = 'F_ALV_COMMA'
        IS_LAYOUT               = WA_LAYOUT
        IT_FIELDCAT             = IT_FIELDCAT
      TABLES
        T_OUTTAB                = IT_ALV
      EXCEPTIONS
        PROGRAM_ERROR           = 1
        OTHERS                  = 2.
    IF SY-SUBRC <> 0.
    ENDIF.

ENDFORM.

FORM F_ALV_COMMA   USING U_UCOMM       TYPE SY-UCOMM
                         US_SELF_FIELD TYPE SLIS_SELFIELD.

  CASE U_UCOMM .
    WHEN '&IC1'. " DUPLO CLICK
      IF US_SELF_FIELD-FIELDNAME EQ 'VBELN'. " VALIDA O CAMPO DO DUPLO CLICK
        READ TABLE IT_ALV INTO WA_ALV INDEX US_SELF_FIELD-TABINDEX.
        IF SY-SUBRC IS INITIAL. "ENCONTRADO O DOCUMENTO QUE FOI SELECIONADO
          IF WA_ALV-VBELN <> ' '.
            SET PARAMETER ID 'AUN' FIELD WA_ALV-VBELN.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
          ELSE.
            MESSAGE: 'Esta requisição encerrou com erro' TYPE 'I'.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.