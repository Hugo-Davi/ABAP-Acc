*&---------------------------------------------------------------------*
*& Include          MZ12AL10_CCOMEX_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS '9000'.
  V_PBO_EXECUTED = ABAP_FALSE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9100 OUTPUT.
  SET PF-STATUS '9100'.
  IF v_pbo_executed <> ABAP_TRUE.
    P_AUART = 'ZORB'.
    P_VKORG = 'BR10'.
  ENDIF.
  P_SPART = '10'.
  P_VTWEG = '10'.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'MATE_TC_INT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE MATE_TC_INT_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE MATE_TC_ITAB LINES MATE_TC_INT-lines.
ENDMODULE.

MODULE NSOL_GET_NEXT_NUMBER OUTPUT.
IF V_PBO_EXECUTED <> ABAP_TRUE.
  DATA: L_STATUS_TELA TYPE C LENGTH 1,
        L_MESSAGE_INICIO TYPE C LENGTH 300.
  CASE SY-DYNNR.
    WHEN '9100'.
      L_STATUS_TELA = '1'.
      L_MESSAGE_INICIO = 'Criação de ordem de venda individual iniciada.'.
    WHEN '9200'.
      L_STATUS_TELA = '2'.
      L_MESSAGE_INICIO = 'Criação de ordem de venda massiva iniciada.'.
  ENDCASE.
  call function 'NUMBER_GET_NEXT'
    exporting
      NR_RANGE_NR                   = '1'
      OBJECT                        = 'ZCOMEX10'
      quantity                      = '0000000001'
    importing
      number                        = p_nsol
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
  IF sy-subrc <> 0.
* Implement suitable error handling here

  ENDIF.
  PERFORM F_LOG USING L_STATUS_TELA
                      L_MESSAGE_INICIO.
ENDIF.
V_PBO_EXECUTED = ABAP_TRUE.
ENDMODULE.