*&---------------------------------------------------------------------*
*& Report ZCOMEX10R_CONSTVENDA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcomex10r_constvenda.

TABLES: ztbcomex_10, ztbcomex_10_log,
        vbak, vbap.

TYPES: BEGIN OF ty_final,
         icon   TYPE C LENGTH 4,
*  STATUS LIKE ZTBCOMEX_10_LOG-STATUS,
         nsol   LIKE ztbcomex_10-nsol,
         vbeln  LIKE vbak-vbeln,
         erdat  LIKE vbak-erdat,
         ernam  LIKE vbak-ernam,
         posnr  LIKE vbap-posnr,
         matnr  LIKE vbap-matnr,
         kwmeng LIKE vbap-kwmeng,
         meins  LIKE vbap-meins,
         pstyv  LIKE vbap-pstyv,
         log    LIKE ztbcomex_10_log-message,
       END OF ty_final.

DATA: it_final TYPE STANDARD TABLE OF ty_final,
      wa_final TYPE ty_final.

DATA: wa_layout   TYPE slis_layout_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      it_fieldcat TYPE slis_t_fieldcat_alv.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_nsol FOR ztbcomex_10-nsol,
                  s_status FOR ztbcomex_10_log-status NO INTERVALS,
                  s_erdat FOR vbak-erdat,
                  s_vbeln FOR vbak-vbeln.
SELECTION-SCREEN END OF BLOCK b1.
*&-------------------------------------&
*&-------------  INICIO  --------------&
*&------------  DA LÓGICA -------------&
*&-------------------------------------&
SELECT log~status, log~message,
       cx~nsol, cx~vbeln,
       vk~erdat, vk~ernam,
       vp~posnr, vp~matnr, vp~kwmeng, vp~meins, vp~pstyv
  FROM ztbcomex_10 AS cx
  INNER JOIN vbak AS vk ON vk~vbeln EQ cx~vbeln
  INNER JOIN vbap AS vp ON vp~vbeln EQ cx~vbeln
  INNER JOIN ztbcomex_10_log AS log ON log~nsol EQ cx~nsol
  INTO TABLE @DATA(it_primeira)
  WHERE CX~NSOL IN @S_NSOL AND
        LOG~STATUS IN @S_STATUS AND
        VK~ERDAT IN @S_ERDAT AND
        CX~VBELN IN @S_VBELN.

IF sy-subrc IS INITIAL.
  " SORT IT_SAIDA ASCENDING BY EBELN.
  LOOP AT it_primeira INTO DATA(wa_primeira).
    CLEAR WA_FINAL.
       WA_FINAL-NSOL = WA_PRIMEIRA-NSOL.
       WA_FINAL-VBELN = WA_PRIMEIRA-VBELN.
       WA_FINAL-ERDAT = WA_PRIMEIRA-ERDAT.
       WA_FINAL-ERNAM = WA_PRIMEIRA-ERNAM.
       WA_FINAL-POSNR = WA_PRIMEIRA-POSNR.
       WA_FINAL-MATNR = WA_PRIMEIRA-MATNR.
       WA_FINAL-KWMENG = WA_PRIMEIRA-KWMENG.
       WA_FINAL-MEINS = WA_PRIMEIRA-MEINS.
       WA_FINAL-PSTYV = WA_PRIMEIRA-PSTYV.
       WA_FINAL-LOG = WA_PRIMEIRA-MESSAGE.
    CASE wa_primeira-status.
      WHEN '1'.
        wa_FINAL-icon = '@09@'.
      WHEN '2'.
        wa_FINAL-icon = '@09@'.
      WHEN '3'.
        wa_FINAL-icon = '@08@'.
      WHEN '4'.
        wa_FINAL-icon = '@08@'.
      WHEN '5'.
        wa_FINAL-icon = '@0A@'.
    ENDCASE.
    APPEND WA_FINAL TO IT_FINAL.
  ENDLOOP.
        PERFORM F_CAMPOS.
        PERFORM F_ALV.
ELSE.
  MESSAGE: 'Não existe dados correspondentes' TYPE 'I'.
ENDIF.
*&---------------------------------------------------------------------*
*& Form campos
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CAMPOS .

*Exibição de titulos, campos e conteudos da tabela.

  WA_FIELDCAT-FIELDNAME = 'ICON'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'Semaforo'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'NSOL'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'N° da Solicitação'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'VBELN'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'N° Ordem de Venda'.
  WA_FIELDCAT-HOTSPOT = ABAP_TRUE.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'ERDAT'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'DT de Criação'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'ERNAM'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'Usuario Criação'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'POSNR'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'Numero Item'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'MATNR'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'Numero Material'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'KWMENG'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'Quant. Item'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'MEINS'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'Unid Medida'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'PSTYV'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'Categ. Item'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'LOG'.
  WA_FIELDCAT-TABNAME   = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_M = 'Log Documento'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

ENDFORM.

FORM F_ALV.

*Layout do ALV
  WA_LAYOUT-ZEBRA = 'X'.
  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = SY-REPID
        I_CALLBACK_USER_COMMAND = 'F_ALV_COMMA'
        IS_LAYOUT               = WA_LAYOUT
        IT_FIELDCAT             = IT_FIELDCAT
      TABLES
        T_OUTTAB                = IT_FINAL
      EXCEPTIONS
        PROGRAM_ERROR           = 1
        OTHERS                  = 2.
    IF SY-SUBRC <> 0.
    ENDIF.

ENDFORM.
*
*
FORM F_ALV_COMMA   USING U_UCOMM       TYPE SY-UCOMM
                         US_SELF_FIELD TYPE SLIS_SELFIELD.

  CASE U_UCOMM .
    WHEN '&IC1'. " DUPLO CLICK
      IF US_SELF_FIELD-FIELDNAME EQ 'VBELN'. " VALIDA O CAMPO DO DUPLO CLICK
        READ TABLE IT_FINAL INTO WA_FINAL INDEX US_SELF_FIELD-TABINDEX.
        IF SY-SUBRC IS INITIAL. "ENCONTRADO O DOCUMENTO QUE FOI SELECIONADO
          SET PARAMETER ID 'AUN' FIELD WA_FINAL-VBELN.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.