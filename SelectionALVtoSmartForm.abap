REPORT zmd_assignmentnew.

TYPE-POOLS: slis,
            sscr.

*----------------------------------------------------------------------*
*                             T A B L E S                              *
*----------------------------------------------------------------------*
TABLES: zmd_depo.   "z table which has 4 area. zmd_urn(matnr type), zmd_mktr(numc10 type), 
                    "zmd_iec(numc1 type connected to domain if its 0 = import, 1 = export), zmd_trh(sydatum type)

*----------------------------------------------------------------------*
*                    I N T E R N A L  T A B L E S                      *
*----------------------------------------------------------------------*

DATA: BEGIN OF itab OCCURS 0,
  mark,
  fcheck        TYPE bus_loca_demo-checkbox,
  zmd_urn       LIKE zmd_depo-zmd_urn,
  zmd_mktr      LIKE zmd_depo-zmd_mktr,
  zmd_iec       LIKE zmd_depo-zmd_iec,
  zmd_trh       LIKE zmd_depo-zmd_trh,
  fprint        TYPE icon-id,
  END OF itab.

DATA gs_itabb LIKE LINE OF itab.
DATA it_depo TYPE TABLE OF zmd_depo.
DATA wa_itab LIKE LINE OF itab.
DATA wa_depo LIKE LINE OF it_depo.
DATA ls_depo LIKE LINE OF it_depo.
DATA it_smo TYPE TABLE OF zmd_depo WITH HEADER LINE.


DATA: s_fcat TYPE slis_fieldcat_alv,
      st_fcat TYPE slis_t_fieldcat_alv.

*----------------------------------------------------------------------*
*                           V A R I A B L E S                          *
*----------------------------------------------------------------------*
*-- ALV Variables - I
DATA gt_fcat1       TYPE slis_t_fieldcat_alv.
DATA gs_fcat1       LIKE LINE OF gt_fcat1.
DATA gs_layout1     TYPE slis_layout_alv .
DATA gs_variant1    TYPE disvariant .
DATA gs_is_grid1    TYPE lvc_s_glay.
DATA gt_event1      TYPE slis_t_event.
DATA gs_event_exit1 TYPE slis_t_event_exit.
DATA gt_sort1       TYPE slis_t_sortinfo_alv.
DATA gs_sort1       LIKE LINE OF gt_sort1.


*----------------------------------------------------------------------*
*                           CONSTANTS                                  *
*----------------------------------------------------------------------*


CONSTANTS gc_icon_green_light(4)  VALUE '@08@'.
CONSTANTS gc_icon_red_light(4)    VALUE '@0A@'.
CONSTANTS gc_icon_yellow_light(4) VALUE '@09@'.
CONSTANTS gc_icon_led_inactive(4) VALUE '@BZ@'.
CONSTANTS gc_icon_xls(4)          VALUE '@J2@'.
CONSTANTS gc_icon_change(4)       VALUE '@0Z@'.
CONSTANTS gc_icon_create(4)       VALUE '@0Y@'.
CONSTANTS gc_icon_print(4)        VALUE '@0X@'.
CONSTANTS c_check(1)              VALUE 'X'.


*----------------------------------------------------------------------*
*                  S E L E C T I O N   S C R E E N                     *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE text-b10.
  SELECT-OPTIONS s_urnkd FOR itab-zmd_urn.
  SELECT-OPTIONS s_date FOR itab-zmd_trh.
  PARAMETERS: p_lstbx TYPE z_iec AS LISTBOX VISIBLE LENGTH 14
OBLIGATORY DEFAULT 1.
SELECTION-SCREEN END OF BLOCK b10.



*----------------------------------------------------------------------*
*                  P R O G R A M    S T A R T                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.


START-OF-SELECTION.
  PERFORM f_get_data.
  PERFORM f_set_data.



*----------------------------------------------------------------------*
*                  END-OF-SELECTION                                    *
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM z_baslikgetir.
  PERFORM f_display_with_alv_010.


*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_WITH_ALV_010
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_display_with_alv_010 .

  DATA ls_event       TYPE slis_alv_event.
*
**-- Set display options
  gs_variant1-report  = sy-cprog .
  gs_layout1-box_fieldname      = 'MARK'.
  gs_layout1-info_fieldname     = 'LINE_COLOR'.
  gs_layout1-zebra              = 'X' .
  gs_layout1-colwidth_optimize  = 'X' .

  ls_event-name = 'DATA_CHANGED'.
  ls_event-form = 'METHOD_DATA_CHANGED'.
  APPEND ls_event TO gt_event1.

  ls_event-name = 'USER_COMMAND'.
  ls_event-form = 'F_ALV_UCOMM_010'.
  APPEND ls_event TO gt_event1.

  ls_event-name = 'PF_STATUS_SET'.
  ls_event-form = 'F_ALV_STATUS_010'.
  APPEND ls_event TO gt_event1.
*
  gs_is_grid1-edt_cll_cb = 'X'.

  IF sy-batch NE 'X'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_grid_settings    = gs_is_grid1
        it_fieldcat        = st_fcat[]
        it_sort            = gt_sort1
        is_layout          = gs_layout1
        is_variant         = gs_variant1
        i_save             = 'X'
        i_callback_program = sy-repid
        it_events          = gt_event1
        it_event_exit      = gs_event_exit1
      TABLES
        t_outtab           = itab.

  ELSE.

    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_grid_settings    = gs_is_grid1
        is_layout   = gs_layout1
        it_events   = gt_event1
        it_fieldcat = st_fcat
      TABLES
        t_outtab    = itab.
  ENDIF.

ENDFORM.

*------------------------------------------------------------------*
*       FORM f_alv_ucomm                                          *
*------------------------------------------------------------------*
FORM f_alv_ucomm_010
USING pv_r_ucomm LIKE sy-ucomm
      ps_rs_selfield TYPE slis_selfield.

  DATA ls_itab LIKE LINE OF itab.

      CASE pv_r_ucomm.
        WHEN '&IC1'.
          IF ps_rs_selfield-fieldname = 'FPRINT'.
            READ TABLE itab INTO ls_itab INDEX ps_rs_selfield-tabindex.
            PERFORM f_call_smo USING ls_itab-zmd_urn.
          ENDIF.
          IF ps_rs_selfield-fieldname = 'ZMD_URN'.
          PERFORM toggle_column_edit_ac.
          PERFORM zrefresh.
          ENDIF.
        WHEN 'PRX'.
          PERFORM zhepsinial.
        WHEN 'EKL'.
          PERFORM zzurunekl.
          PERFORM zrefresh.
          PERFORM f_get_data.
          PERFORM f_set_data.
*          PERFORM toggle_column_edit_ac.
*          CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_REFRESH'.
*          .
*          PERFORM f_display_with_alv_020.
        WHEN 'KYD'.
          PERFORM zkaydettir.
          CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_REFRESH'.
          .
          PERFORM toggle_column_edit_kapa.
          PERFORM f_display_with_alv_020.
        ENDCASE.

      ps_rs_selfield-refresh     = 'X'.
      ps_rs_selfield-col_stable  = 'X'.
      ps_rs_selfield-row_stable  = 'X'.
ENDFORM . "user_command


*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM f_get_data .
SELECT * FROM zmd_depo INTO CORRESPONDING FIELDS OF TABLE itab
  WHERE zmd_urn IN s_urnkd
  AND zmd_iec = p_lstbx
  AND zmd_trh IN s_date.
ENDFORM.

FORM zrefresh.
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_REFRESH'.
          .
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_SET_DATA
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM f_set_data .
  DATA ls_itab LIKE LINE OF itab.
  LOOP AT itab INTO ls_itab.
    ls_itab-fprint = gc_icon_print.
    MODIFY itab FROM ls_itab.
  ENDLOOP.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_CALL_SMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LSTJ_EBELN  text
*----------------------------------------------------------------------*
FORM f_call_smo USING zmd_prmtr.
  DATA lv_formname            TYPE tdsfname VALUE 'ZMD_SMO1'.
  DATA lv_fm_name             TYPE rs38l_fnam.
  DATA ls_control_parameters  TYPE ssfctrlop.
  DATA ls_errortab            TYPE tsferror.


  PERFORM f_ssf_function_module_name
  USING lv_formname
  CHANGING lv_fm_name.

  PERFORM f_print_settings USING ls_control_parameters.
  PERFORM f_ssf_open USING ls_control_parameters.
  PERFORM f_ssf_print
  USING ls_control_parameters
        lv_fm_name
        zmd_prmtr.

  PERFORM f_ssf_close.

  PERFORM f_ssf_read_errors
  USING ls_errortab.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_SSF_PRINT
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM f_ssf_print
USING ps_control_parameters TYPE ssfctrlop
      pv_fm_name   TYPE  rs38l_fnam
      zmd_prmtr.

  DATA ls_head  TYPE zmd_itsmo.
  DATA lt_items TYPE TABLE OF zmd_itsmo WITH HEADER LINE.
  DATA ls_items TYPE zmd_itsmo.

  LOOP AT itab .
      IF itab-zmd_urn = zmd_prmtr.
         lt_items-zmd_urn  = itab-zmd_urn.
         lt_items-zmd_mktr = itab-zmd_mktr.
         lt_items-zmd_iec  = itab-zmd_iec.
         lt_items-zmd_trh  = itab-zmd_trh.
         APPEND lt_items.
      ENDIF.
  ENDLOOP.

*-- Call the generated function module
  CALL FUNCTION pv_fm_name
    EXPORTING
      control_parameters = ps_control_parameters
      gs_head            = ls_head
    TABLES
      gt_items           = lt_items
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. " F_SSF_PRINT


FORM zhepsinial.
  DATA lt_items TYPE TABLE OF zmd_itsmo WITH HEADER LINE.
  DATA ls_items TYPE zmd_itsmo.

  LOOP AT itab.
     IF itab-fcheck = 'X'.
        lt_items-zmd_urn  = itab-zmd_urn.
        lt_items-zmd_mktr = itab-zmd_mktr.
        lt_items-zmd_iec  = itab-zmd_iec.
        lt_items-zmd_trh  = itab-zmd_trh.
        APPEND lt_items.
     ENDIF.
  ENDLOOP.

  CALL FUNCTION '/1BCDWB/SF00000327'
    EXPORTING
      gs_head                    = lt_items
    TABLES
      gt_items                   = lt_items
   EXCEPTIONS
     formatting_error           = 1
     internal_error             = 2
     send_error                 = 3
     user_canceled              = 4
     OTHERS                     = 5.
ENDFORM.

FORM zkaydettir.

  READ TABLE itab INTO wa_itab INDEX 1.
  wa_depo-zmd_urn = wa_itab-zmd_urn.
  wa_depo-zmd_mktr = wa_itab-zmd_mktr.
  wa_depo-zmd_iec = wa_itab-zmd_iec.
  wa_depo-zmd_trh = wa_itab-zmd_trh.

  MODIFY zmd_depo FROM wa_depo.
  DELETE FROM zmd_depo WHERE zmd_urn = space AND zmd_mktr = space.
  PERFORM f_get_data.
  PERFORM f_set_data.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ZZURUNEKL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zzurunekl.

  CLEAR ls_depo.
  ls_depo-zmd_urn   = space.
  ls_depo-zmd_mktr  = space.
  ls_depo-zmd_trh   = sy-datum.

  IF itab-zmd_iec = '1'.
    ls_depo-zmd_iec = '1'.
  ELSE.
    ls_depo-zmd_iec = '2'.
  ENDIF.
  MODIFY zmd_depo FROM ls_depo.

*  PERFORM f_get_data.
*  PERFORM f_set_data.

ENDFORM.


FORM toggle_column_edit_ac.
  FIELD-SYMBOLS <s_fcat> LIKE LINE OF st_fcat.
  LOOP AT st_fcat ASSIGNING <s_fcat>.
    IF <s_fcat>-fieldname = 'ZMD_URN' OR <s_fcat>-fieldname =
'ZMD_MKTR'.
      <s_fcat>-edit = 'X'.
    ENDIF.
  ENDLOOP.
  PERFORM f_display_with_alv_010 .
ENDFORM. " toggle_column_edit


FORM toggle_column_edit_kapa.
  FIELD-SYMBOLS <s_fcat> LIKE LINE OF st_fcat.
  LOOP AT st_fcat ASSIGNING <s_fcat>.
    IF <s_fcat>-fieldname = 'ZMD_URN' OR <s_fcat>-fieldname =
'ZMD_MKTR'.
      <s_fcat>-edit = ' '.
    ENDIF.
  ENDLOOP.
  PERFORM f_display_with_alv_010.
ENDFORM. " toggle_column_edit


FORM f_display_with_alv_020 .

  DATA ls_event       TYPE slis_alv_event.
*
**-- Set display options
  gs_variant1-report  = sy-cprog .
  gs_layout1-box_fieldname      = 'MARK'.
  gs_layout1-info_fieldname     = 'LINE_COLOR'.
  gs_layout1-zebra              = 'X' .
  gs_layout1-colwidth_optimize  = 'X' .

  ls_event-name = 'DATA_CHANGED'.
  ls_event-form = 'METHOD_DATA_CHANGED'.
  APPEND ls_event TO gt_event1.

  ls_event-name = 'USER_COMMAND'.
  ls_event-form = 'F_ALV_UCOMM_010'.
  APPEND ls_event TO gt_event1.

  ls_event-name = 'PF_STATUS_SET'.
  ls_event-form = 'F_ALV_STATUS_010'.
  APPEND ls_event TO gt_event1.
*
  gs_is_grid1-edt_cll_cb = 'X'.
  PERFORM f_get_data.
  PERFORM f_set_data.
  IF sy-batch NE 'X'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_grid_settings    = gs_is_grid1
        it_fieldcat        = st_fcat[]
        it_sort            = gt_sort1
        is_layout          = gs_layout1
        is_variant         = gs_variant1
        i_save             = 'A'
        i_callback_program = sy-repid
        it_events          = gt_event1
        it_event_exit      = gs_event_exit1
      TABLES
        t_outtab           = itab.

  ELSE.

    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_grid_settings    = gs_is_grid1
        is_layout   = gs_layout1
        it_events   = gt_event1
        it_fieldcat = st_fcat
      TABLES
        t_outtab    = itab.
  ENDIF.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  Z_BASLIKGETIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_baslikgetir .
  CLEAR s_fcat.
  s_fcat-col_pos    = 1.
  s_fcat-fieldname  = 'FCHECK'.
  s_fcat-seltext_l  = 'Sec'.
  s_fcat-checkbox   = 'X'.
  s_fcat-edit       = 'X'.
  s_fcat-outputlen  = 2.
  APPEND s_fcat TO st_fcat.

  CLEAR s_fcat.
  s_fcat-col_pos   = 2 .
  s_fcat-fieldname = 'ZMD_URN'.
  s_fcat-seltext_l = 'Urun Kodu'.
  s_fcat-outputlen = '10'.
  APPEND s_fcat TO st_fcat.

  CLEAR s_fcat.
  s_fcat-col_pos   = 3 .
  s_fcat-fieldname = 'ZMD_MKTR'.
  s_fcat-seltext_l = 'Miktar'.
  s_fcat-outputlen = '20'.
  APPEND s_fcat TO st_fcat.

  CLEAR s_fcat.
  s_fcat-col_pos   = 4 .
  s_fcat-fieldname = 'ZMD_IEC'.
  s_fcat-seltext_l = 'I/E Durumu'.
  s_fcat-outputlen = '20'.
  APPEND s_fcat TO st_fcat.

  CLEAR s_fcat.
  s_fcat-col_pos   = 5 .
  s_fcat-fieldname = 'ZMD_TRH'.
  s_fcat-seltext_l = 'Giriş Tarihi'.
  s_fcat-outputlen = '20'.
  APPEND s_fcat TO st_fcat.

  CLEAR s_fcat.
  s_fcat-col_pos   = 7 .
  s_fcat-fieldname = 'FPRINT'.
  s_fcat-seltext_l = 'Urun Kodu'.
  s_fcat-outputlen = '10'.
  s_fcat-hotspot   = 'X' .
  APPEND s_fcat TO st_fcat.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_SSF_FUNCTION_MODULE_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FORMNAME  text
*      <--P_LV_FM_NAME  text
*----------------------------------------------------------------------*
FORM f_ssf_function_module_name
USING    pv_formname TYPE tdsfname
CHANGING pv_fm_name  TYPE rs38l_fnam.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = pv_formname
    IMPORTING
      fm_name            = pv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  F_PRINT_SETTINGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_CONTROL_PARAMETERS  text
*----------------------------------------------------------------------*
FORM f_print_settings
USING ps_control_parameters TYPE ssfctrlop.
  ps_control_parameters-preview   = 'X'.
  ps_control_parameters-no_open   = 'X'.
  ps_control_parameters-no_close  = 'X'.

ENDFORM. " F_PRINT_SETTINGS



*&---------------------------------------------------------------------*
*&      Form  F_SSF_OPEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_CONTROL_PARAMETERS  text
*----------------------------------------------------------------------*
FORM f_ssf_open
USING ps_control_parameters TYPE ssfctrlop.
  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = ps_control_parameters
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.
ENDFORM. " F_SSF_OPEN



*&---------------------------------------------------------------------*
*&      Form  F_SSF_CLOSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ssf_close .

  CALL FUNCTION 'SSF_CLOSE'
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. " F_SSF_CLOSE



*&---------------------------------------------------------------------*
*&      Form  F_SSF_READ_ERRORS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      -->P_LS_ERRORTAB  text
*----------------------------------------------------------------------*
FORM f_ssf_read_errors
USING ps_errortab TYPE tsferror.
  CALL FUNCTION 'SSF_READ_ERRORS'
    IMPORTING
      errortab = ps_errortab.
ENDFORM. " F_SSF_READ_ERRORS


FORM method_data_changed
     USING lrc_i_dc TYPE REF TO cl_alv_changed_data_protocol.
ENDFORM.




FORM f_alv_status_010 USING x.
  SET PF-STATUS 'ZSTANDARD'.
ENDFORM. "F_ALV_STATUS
