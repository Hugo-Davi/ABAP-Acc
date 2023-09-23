*&---------------------------------------------------------------------*
*& Include          MZ12AL10_CCOMEX_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'P_VENDAINDV'.
      CALL TRANSACTION 'ZRE13AL10_VENDAINDV'. " SCREEN 9100
    WHEN 'P_VENDAMASS'.
      CALL TRANSACTION 'ZRE13AL10_VENDAMASS'. " SCREEN 9200
    WHEN 'P_CONSTVENDA'.
      CALL TRANSACTION 'ZRE13AL10_CONSTVENDA'." SCREEN 9300
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      PERFORM F_LOG USING '3'
                          'Criação de ordem de venda cancelada.'.

      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      PERFORM F_LOG USING '3'
                          'Criação de ordem de venda cancelada.'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM F_BAPI_CRIAR.
      LEAVE PROGRAM.

  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'MATE_TC_INT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE MATE_TC_INT_MODIFY INPUT.
  MODIFY MATE_TC_ITAB
    FROM MATE_TC_WA
    INDEX MATE_TC_INT-CURRENT_LINE.
    " Salvando na tabela interna
    IF SY-SUBRC IS NOT INITIAL.
        IF STRLEN( MATE_TC_WA-MATNR ) < 18.
           DATA(lv_zeros_to_add) = 18 - STRLEN( MATE_TC_WA-MATNR ).
           DO LV_ZEROS_TO_ADD TIMES.
             MATE_TC_WA-MATNR = '0' && MATE_TC_WA-MATNR.
           ENDDO.
        ENDIF.
*-------------------------
     " ATRIBUINDO DESCRIÇÃO
     SELECT SINGLE ARKTX
       FROM VBAP  " Busca Descrição na tabela VBAP
       WHERE MATNR EQ @MATE_TC_WA-MATNR
       INTO @MATE_TC_WA-ARKTX.
     IF SY-SUBRC IS NOT INITIAL.
       SELECT SINGLE MAKTX
         FROM MAKT  " Se não for encontrada, é procurada na tabela MAKT
         WHERE MATNR EQ @MATE_TC_WA-MATNR
         INTO @MATE_TC_WA-ARKTX.
       IF SY-SUBRC IS NOT INITIAL.
       ENDIF.
     ENDIF.
* ---------------------------
     INSERT MATE_TC_WA INTO TABLE MATE_TC_ITAB.
    ENDIF.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'MATE_TC_INT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE MATE_TC_INT_MARK INPUT.
  DATA: g_MATE_TC_INT_wa2 like line of MATE_TC_ITAB.
    if MATE_TC_INT-line_sel_mode = 1
    and MATE_TC_WA-MARK = 'X'.
     loop at MATE_TC_ITAB into g_MATE_TC_INT_wa2
       where MARK = 'X'.
       g_MATE_TC_INT_wa2-MARK = ''.
       modify MATE_TC_ITAB
         from g_MATE_TC_INT_wa2
         transporting MARK.
     endloop.
  endif.
  MODIFY MATE_TC_ITAB
    FROM MATE_TC_WA
    INDEX MATE_TC_INT-CURRENT_LINE
    TRANSPORTING MARK.
ENDMODULE.

MODULE MATE_TC_INT_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'MATE_TC_INT'
                              'MATE_TC_ITAB'
                              'MARK'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.