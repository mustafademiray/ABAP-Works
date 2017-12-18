FUNCTION zui_mm03_rq_mas.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IR_MATNR) TYPE  RANGE_T_MATNR OPTIONAL
*"     VALUE(IR_PRDHA) TYPE  ZUI_T_RANGE_PRDHA OPTIONAL
*"     VALUE(MAX_HITS) TYPE  I OPTIONAL
*"  TABLES
*"      ET_LIST10 STRUCTURE  ZUI_S_MM03_RQ_MAS OPTIONAL
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
  DATA lt_list10 TYPE TABLE OF zui_s_mm03_rq_mas WITH HEADER LINE.
  DATA ls_list10 LIKE LINE OF lt_list10.
  DATA temp TYPE string.
  DATA:it_line  LIKE tline OCCURS 0 WITH HEADER LINE,
  v_tdname LIKE thead-tdname.
  DATA:it_line2 LIKE tline OCCURS 0 WITH HEADER LINE.
  DATA matnrx(18) TYPE c.

  DATA: BEGIN OF lt_line OCCURS 0,
       matnr    TYPE mara-matnr,
       tdline   TYPE string,
      END OF lt_line.

  DATA: BEGIN OF lt_temp OCCURS 0,
       matnr    TYPE mara-matnr,
       temp     TYPE string,
      END OF lt_temp.

  DATA ls_temp LIKE LINE OF lt_temp.

  DATA : ls_line  LIKE LINE OF lt_line.
  DATA : lt_line2 LIKE TABLE OF lt_line WITH HEADER LINE.
  DATA : ls_line2 LIKE LINE OF lt_line2.
  DATA : is_line  LIKE LINE OF it_line.

  DATA: BEGIN OF lt_mara OCCURS 0,
    matnr LIKE mara-matnr,
    matkl LIKE mara-matkl,
    ean11 LIKE mara-ean11,
    mfrpn LIKE mara-mfrpn,
    mfrnr LIKE mara-mfrnr,
    extwg LIKE mara-extwg,
    wgbez LIKE t023t-wgbez,
    ewbez LIKE twewt-ewbez,
    maktx LIKE makt-maktx,
    ktgrm LIKE mvke-ktgrm,
    vtext LIKE tvkmt-vtext,
    prdha LIKE mara-prdha,
  END OF lt_mara.
  DATA : BEGIN OF lt_t023t OCCURS 0,
    matkl LIKE t023t-matkl,
    wgbez LIKE t023t-wgbez,
  END OF lt_t023t.
  DATA : BEGIN OF lt_twewt OCCURS 0,
    extwg LIKE twewt-extwg,
    ewbez LIKE twewt-ewbez,
  END OF lt_twewt.
  DATA : BEGIN OF lt_makt OCCURS 0,
    matnr LIKE makt-matnr,
    maktx LIKE makt-maktx,
  END OF lt_makt.
  DATA : BEGIN OF lt_mvke OCCURS 0,
    matnr LIKE mvke-matnr,
    ktgrm LIKE mvke-ktgrm,
    vtext LIKE tvkmt-vtext,
    vkorg LIKE mvke-vkorg,
    vtweg LIKE mvke-vtweg,
    END OF lt_mvke.
  DATA : BEGIN OF lt_tvkmt OCCURS 0,
    ktgrm LIKE tvkmt-ktgrm,
    vtext LIKE tvkmt-vtext,
    END OF lt_tvkmt.
  DATA : BEGIN OF lt_marc OCCURS 0,
    matnr LIKE marc-matnr,
    prctr LIKE marc-prctr,
    sernp LIKE marc-sernp,
    werks LIKE marc-werks,
    kokrs LIKE tka02-kokrs,
    ktext LIKE cepct-ktext,
    verpr LIKE mbew-verpr,
    verpr2 LIKE ckmlcr-pvprs,
    verpr3 LIKE ckmlcr-pvprs,
    END OF lt_marc.
  DATA : BEGIN OF lt_t001k OCCURS 0,
    bwkey LIKE t001k-bwkey,
    bukrs LIKE t001k-bukrs,
    kokrs LIKE tka02-kokrs,
    END OF lt_t001k.
  DATA : BEGIN OF lt_tka02 OCCURS 0,
    bukrs LIKE tka02-bukrs,
    kokrs LIKE tka02-kokrs,
    END OF lt_tka02.
  DATA : BEGIN OF lt_cepct OCCURS 0,
    prctr LIKE cepct-prctr,
    ktext LIKE cepct-ktext,
    END OF lt_cepct.
  DATA : BEGIN OF lt_mbew OCCURS 0,
    matnr  LIKE mbew-matnr,
    bwkey  LIKE mbew-bwkey,
    verpr  LIKE mbew-verpr,
    kaln1  LIKE mbew-kaln1,
    verpr2 LIKE ckmlcr-pvprs,
    verpr3 LIKE ckmlcr-pvprs,
    END OF lt_mbew.
  DATA : BEGIN OF lt_ckml OCCURS 0,
    pvprs LIKE ckmlcr-pvprs,
    bdatj LIKE ckmlcr-bdatj,
    poper LIKE ckmlcr-poper,
    curtp LIKE ckmlcr-curtp,
    kalnr LIKE ckmlcr-kalnr,
    END OF lt_ckml.


  DATA : ls_mara  LIKE LINE OF lt_mara,
         ls_t023t LIKE LINE OF lt_t023t,
         ls_twewt LIKE LINE OF lt_twewt,
         ls_makt  LIKE LINE OF lt_makt,
         ls_mvke  LIKE LINE OF lt_mvke,
         ls_tvkmt LIKE LINE OF lt_tvkmt,
         ls_marc  LIKE LINE OF lt_marc,
         ls_t001k LIKE LINE OF lt_t001k,
         ls_tka02 LIKE LINE OF lt_tka02,
         ls_cepct LIKE LINE OF lt_cepct,
         ls_mbew  LIKE LINE OF lt_mbew,
         ls_ckml  LIKE LINE OF lt_ckml.

  CHECK ir_matnr IS NOT INITIAL.

  SELECT
    matnr
    matkl
    ean11
    mfrpn
    mfrnr
    extwg
    prdha
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    UP TO max_hits ROWS
    WHERE matnr IN ir_matnr AND
          prdha IN ir_prdha.

  CHECK lt_mara[] IS NOT INITIAL.
  IF lt_mara[] IS NOT INITIAL.
    SELECT
    matkl
    wgbez
    FROM t023t
    INTO CORRESPONDING FIELDS OF TABLE lt_t023t
    FOR ALL ENTRIES IN lt_mara
    WHERE matkl EQ lt_mara-matkl
    AND   spras EQ sy-langu.

    SELECT
    extwg
    ewbez
    FROM twewt
    INTO CORRESPONDING FIELDS OF TABLE lt_twewt
    FOR ALL ENTRIES IN lt_mara
    WHERE extwg = lt_mara-extwg
    AND   spras = sy-langu.

    SELECT
      matnr
      maktx
      FROM makt
      INTO CORRESPONDING FIELDS OF TABLE lt_makt
      FOR ALL ENTRIES IN lt_mara
      WHERE matnr = lt_mara-matnr
      AND   spras = sy-langu.

    LOOP AT lt_mara INTO ls_mara.
      READ TABLE lt_t023t INTO ls_t023t WITH KEY matkl = ls_mara-matkl.
      ls_mara-wgbez = ls_t023t-wgbez.
      READ TABLE lt_twewt INTO ls_twewt WITH KEY extwg = ls_mara-extwg.
      ls_mara-ewbez = ls_twewt-ewbez.
      READ TABLE lt_makt INTO ls_makt   WITH KEY matnr = ls_mara-matnr.
      ls_mara-maktx = ls_makt-maktx.
      MODIFY lt_mara FROM ls_mara.
      CLEAR : ls_mara , ls_t023t, ls_twewt, ls_makt.
    ENDLOOP.

  ENDIF.

  SELECT
  matnr
  ktgrm
  vkorg
  vtweg
  FROM mvke
  INTO CORRESPONDING FIELDS OF TABLE lt_mvke
  FOR ALL ENTRIES IN lt_mara
  WHERE matnr = lt_mara-matnr
  AND   vkorg = '1000'
  AND   vtweg = '05'.

  IF lt_mvke[] IS NOT INITIAL.
    SELECT
    ktgrm
    vtext
    FROM tvkmt
    INTO CORRESPONDING FIELDS OF TABLE lt_tvkmt
    FOR ALL ENTRIES IN lt_mvke
    WHERE ktgrm = lt_mvke-ktgrm
    AND   spras = sy-langu.
  ENDIF.

  LOOP AT lt_mvke INTO ls_mvke.
    READ TABLE lt_tvkmt INTO ls_tvkmt WITH KEY ktgrm = ls_mvke-ktgrm.
    ls_mvke-vtext = ls_tvkmt-vtext.
    MODIFY lt_mvke FROM ls_mvke.
  ENDLOOP.

  LOOP AT lt_mara INTO ls_mara.
    READ TABLE lt_mvke INTO ls_mvke WITH KEY matnr = ls_mara-matnr.
    ls_mara-ktgrm = ls_mvke-ktgrm.
    ls_mara-vtext = ls_mvke-vtext.
    MODIFY lt_mara FROM ls_mara.
    CLEAR : ls_mara, ls_mvke.
  ENDLOOP.

  SELECT
    matnr
    prctr
    sernp
    werks
    FROM marc
    INTO CORRESPONDING FIELDS OF TABLE lt_marc
    FOR ALL ENTRIES IN lt_mara
    WHERE matnr = lt_mara-matnr
    AND   werks = '1000'.
**** ktext çekilebilmesi için kokrs hesaplaması
  SELECT
  bwkey
  bukrs
    FROM t001k
   INTO CORRESPONDING FIELDS OF TABLE lt_t001k
   WHERE bwkey = '1000'.

  SELECT
    bukrs
    kokrs
    FROM  tka02
    INTO CORRESPONDING FIELDS OF TABLE lt_tka02
    FOR ALL ENTRIES IN lt_t001k
    WHERE bukrs = lt_t001k-bukrs
    AND   gsber = space.

  LOOP AT lt_t001k INTO ls_t001k.
    READ TABLE lt_tka02 INTO ls_tka02 WITH KEY bukrs = ls_t001k-bukrs.
    ls_t001k-kokrs = ls_tka02-kokrs.
    MODIFY lt_t001k FROM ls_t001k.
    CLEAR : ls_t001k, ls_tka02.
  ENDLOOP.

  LOOP AT lt_marc INTO ls_marc.
    READ TABLE lt_t001k INTO ls_t001k WITH KEY bwkey = ls_marc-werks.
    ls_marc-kokrs = ls_t001k-kokrs.
    MODIFY lt_marc FROM ls_marc.
    CLEAR : ls_t001k, ls_marc.
  ENDLOOP.

  SELECT
    prctr
    ktext
    FROM cepct
    INTO CORRESPONDING FIELDS OF TABLE lt_cepct
    FOR ALL ENTRIES IN lt_marc
    WHERE prctr = lt_marc-prctr
    AND   kokrs = lt_marc-kokrs
    AND   spras = sy-langu.

  SELECT
    matnr
    bwkey
    verpr
    kaln1
    FROM mbew
    INTO CORRESPONDING FIELDS OF TABLE lt_mbew
    FOR ALL ENTRIES IN lt_marc
    WHERE matnr = lt_marc-matnr
    AND   bwkey = lt_marc-werks.

  SELECT
    kalnr
    pvprs
    bdatj
    poper
    curtp
   FROM ckmlcr
    INTO CORRESPONDING FIELDS OF TABLE lt_ckml
    FOR ALL ENTRIES IN lt_mbew
    WHERE kalnr = lt_mbew-kaln1
    AND   curtp IN ('30' , '40').

  SORT lt_ckml BY kalnr DESCENDING bdatj DESCENDING poper DESCENDING.

  LOOP AT lt_mbew INTO ls_mbew.
    READ TABLE lt_ckml INTO ls_ckml WITH KEY kalnr = ls_mbew-kaln1
                                       curtp = '30' .
    ls_mbew-verpr2 = ls_ckml-pvprs.
    READ TABLE lt_ckml INTO ls_ckml WITH KEY kalnr = ls_mbew-kaln1
                                             curtp = '40'.
    ls_mbew-verpr3 = ls_ckml-pvprs.
    MODIFY lt_mbew FROM ls_mbew.
    CLEAR : ls_mbew, ls_ckml.
  ENDLOOP.

  LOOP AT lt_marc INTO ls_marc.
    READ TABLE lt_mbew INTO ls_mbew WITH KEY matnr = ls_marc-matnr
                                             bwkey = ls_marc-werks.
    ls_marc-verpr  = ls_mbew-verpr.
    ls_marc-verpr2 = ls_mbew-verpr2.
    ls_marc-verpr3 = ls_mbew-verpr3.

    READ TABLE lt_cepct INTO ls_cepct WITH KEY prctr = ls_marc-prctr.

    ls_marc-ktext = ls_cepct-ktext.
    MODIFY lt_marc FROM ls_marc.
    CLEAR : ls_marc, ls_cepct,ls_mbew.
  ENDLOOP.

  LOOP AT lt_mvke INTO ls_mvke.
    CONCATENATE ls_mvke-matnr ls_mvke-vkorg ls_mvke-vtweg INTO v_tdname RESPECTING BLANKS.
    CLEAR temp.
    "sipariş metni
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = '0001'
        language                = sy-langu
        name                    = v_tdname
        object                  = 'MVKE'
        archive_handle          = 0
      TABLES
        lines                   = it_line
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.

    ELSE.
      LOOP AT it_line.
        PERFORM tab_to_string TABLES it_line
                              CHANGING temp.
        MOVE temp TO lt_line-tdline.
        MOVE ls_mvke-matnr  TO lt_line-matnr.
        APPEND lt_line.
      ENDLOOP.
    ENDIF.

    CLEAR v_tdname.
    "satınalma sipariş metni
    v_tdname = ls_mvke-matnr.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = 'BEST'
        language                = sy-langu
        name                    = v_tdname
        object                  = 'MATERIAL'
        archive_handle          = 0
      TABLES
        lines                   = it_line2
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
    ELSE.
      LOOP AT it_line2.
        PERFORM tab_to_string TABLES it_line2
                               CHANGING temp.
        MOVE temp TO lt_line2-tdline.
        MOVE ls_mvke-matnr  TO lt_line2-matnr.
        APPEND lt_line2.
      ENDLOOP.
    ENDIF.
    CLEAR v_tdname.
    CLEAR ls_mvke.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM lt_line COMPARING ALL FIELDS.
  DELETE ADJACENT DUPLICATES FROM lt_line2 COMPARING ALL FIELDS.
  CLEAR : ls_mara, ls_marc, ls_line, ls_line2.
  LOOP AT lt_mara INTO ls_mara.
    ls_list10-matnr  = ls_mara-matnr.
    ls_list10-matkl  = ls_mara-matkl.
    ls_list10-wgbez  = ls_mara-wgbez.
    ls_list10-extwg  = ls_mara-extwg.
    ls_list10-ewbez  = ls_mara-ewbez.
    ls_list10-ean11  = ls_mara-ean11.
    ls_list10-mfrpn  = ls_mara-mfrpn.
    ls_list10-mfrnr  = ls_mara-mfrnr.
    ls_list10-maktx  = ls_mara-maktx.
    ls_list10-ktgrm  = ls_mara-ktgrm.
    ls_list10-vtext  = ls_mara-vtext.
    ls_list10-prdha  = ls_mara-prdha.
    READ TABLE lt_marc INTO ls_marc WITH KEY matnr = ls_mara-matnr.
    ls_list10-prctr  = ls_marc-prctr.
    ls_list10-sernp  = ls_marc-sernp.
    ls_list10-ktext  = ls_marc-ktext.
    ls_list10-verpr  = ls_marc-verpr.
    ls_list10-VERPR2 = ls_marc-verpr2.
    ls_list10-VERPR3 = ls_marc-verpr3.
    READ TABLE lt_line INTO ls_line WITH KEY matnr = ls_mara-matnr.
    ls_list10-outtext = ls_line-tdline.

    READ TABLE lt_line2 INTO ls_line2 WITH KEY matnr = ls_mara-matnr.
    ls_list10-outtext2 = ls_line2-tdline.
    APPEND ls_list10 TO lt_list10.
    CLEAR : ls_list10, ls_mara, ls_marc, ls_line, ls_line2.
  ENDLOOP.

  et_list10[] = lt_list10[].



ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  tab_to_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_TEXT    text
*      <--P_STR      text
*----------------------------------------------------------------------*
FORM tab_to_string TABLES pt_text STRUCTURE tline
                  CHANGING p_str   TYPE string.
  DATA: ps_text LIKE LINE OF pt_text,
        line    TYPE i VALUE 132,
        lv_len  TYPE i,
        lv_len2  TYPE i,
        lv_len3  TYPE i.
  CLEAR p_str.
  LOOP AT pt_text INTO ps_text.
    IF sy-tabix > 1 .
      lv_len = STRLEN( p_str ) - 1.
      lv_len2 = lv_len + 1.
      lv_len3 = line * ( sy-tabix - 1 ).
      IF sy-tabix > 1   AND
        lv_len2 NE lv_len3  AND
        ( p_str+lv_len(1) NE space AND ps_text(1) NE space ).
        CONCATENATE p_str ps_text-tdline INTO p_str SEPARATED BY space.
      ELSE.
        CONCATENATE p_str ps_text-tdline INTO p_str.
      ENDIF.
    ELSE.
      CONCATENATE p_str ps_text-tdline INTO p_str.
    ENDIF.

  ENDLOOP.
ENDFORM.                    "tab_to_string
