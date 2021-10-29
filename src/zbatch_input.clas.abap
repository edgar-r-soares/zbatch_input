class ZBATCH_INPUT definition
  public
  final
  create public .

public section.

  class-methods convert_return_tab
    importing
      !it_return type any table
    exporting
      !et_return type standard table .
  methods bdc_dynpro
    importing
      !program type any
      !dynpro type any .
  methods bdc_field
    importing
      !fnam type any
      !fval type any .
  methods constructor
    importing
      !nodata type char1 default '/' .
  methods execute
    importing
      !tcode type sy-tcode
      !mode type ctu_params-dismode default 'E'
      !update type ctu_params-updmode default 'S'
    returning
      value(messtab) type tab_bdcmsgcoll .
  class-methods display_return_messages
    importing
      !it_return type any table .
  methods refresh_bdc .
protected section.
private section.

  data bdcdata type bdcdata_tab .
  data nodata type char1 value '/'. "#EC NOTEXT .
ENDCLASS.



CLASS ZBATCH_INPUT IMPLEMENTATION.


  METHOD bdc_dynpro.
    DATA: ls_bdcdata TYPE bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-program  = program.
    ls_bdcdata-dynpro   = dynpro.
    ls_bdcdata-dynbegin = 'X'.
    APPEND ls_bdcdata TO bdcdata.
  ENDMETHOD.


METHOD bdc_field.
  DATA: ls_bdcdata TYPE bdcdata.
  IF fval <> nodata.
    ls_bdcdata-fnam = fnam.
    ls_bdcdata-fval = fval.
    APPEND ls_bdcdata TO bdcdata.
  ENDIF.
ENDMETHOD.


  METHOD constructor.
    IF nodata IS SUPPLIED.
      me->nodata = nodata.
    ENDIF.
  ENDMETHOD.


METHOD convert_return_tab.
  FIELD-SYMBOLS: <ls_in>  TYPE any,
                 <ls_out> TYPE any,
                 <l_in>   TYPE any,
                 <l_out>  TYPE any.
  DATA: ls_s_msg TYPE bal_s_msg.

  LOOP AT it_return ASSIGNING <ls_in>.
    APPEND INITIAL LINE TO et_return ASSIGNING <ls_out>.

    CLEAR ls_s_msg.

*read original line
    ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_in> TO <l_in>.
    IF sy-subrc EQ 0.
      ls_s_msg-msgid = <l_in>.
    ELSE.
      ASSIGN COMPONENT 'MSGID' OF STRUCTURE <ls_in> TO <l_in>.
      IF sy-subrc EQ 0.
        ls_s_msg-msgid = <l_in>.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGID' OF STRUCTURE <ls_in> TO <l_in>.
        IF sy-subrc EQ 0.
          ls_s_msg-msgid = <l_in>.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_in> TO <l_in>.
    IF sy-subrc EQ 0.
      ls_s_msg-msgty = <l_in>.
    ELSE.
      ASSIGN COMPONENT 'MSGTYP' OF STRUCTURE <ls_in> TO <l_in>.
      IF sy-subrc EQ 0.
        ls_s_msg-msgty = <l_in>.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGTYP' OF STRUCTURE <ls_in> TO <l_in>.
        IF sy-subrc EQ 0.
          ls_s_msg-msgty = <l_in>.
        ELSE.
          ASSIGN COMPONENT 'MSGTY' OF STRUCTURE <ls_in> TO <l_in>.
          IF sy-subrc EQ 0.
            ls_s_msg-msgty = <l_in>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'NUMBER' OF STRUCTURE <ls_in> TO <l_in>.
    IF sy-subrc EQ 0.
      ls_s_msg-msgno = <l_in>.
    ELSE.
      ASSIGN COMPONENT 'MSGNR' OF STRUCTURE <ls_in> TO <l_in>.
      IF sy-subrc EQ 0.
        ls_s_msg-msgno = <l_in>.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGNR' OF STRUCTURE <ls_in> TO <l_in>.
        IF sy-subrc EQ 0.
          ls_s_msg-msgno = <l_in>.
        ELSE.
          ASSIGN COMPONENT 'MSGNO' OF STRUCTURE <ls_in> TO <l_in>.
          IF sy-subrc EQ 0.
            ls_s_msg-msgno = <l_in>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'MESSAGE_V1' OF STRUCTURE <ls_in> TO <l_in>.
    IF sy-subrc EQ 0.
      ls_s_msg-msgv1 = <l_in>.
    ELSE.
      ASSIGN COMPONENT 'MSGV1' OF STRUCTURE <ls_in> TO <l_in>.
      IF sy-subrc EQ 0.
        ls_s_msg-msgv1 = <l_in>.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGV1' OF STRUCTURE <ls_in> TO <l_in>.
        IF sy-subrc EQ 0.
          ls_s_msg-msgv1 = <l_in>.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'MESSAGE_V2' OF STRUCTURE <ls_in> TO <l_in>.
    IF sy-subrc EQ 0.
      ls_s_msg-msgv2 = <l_in>.
    ELSE.
      ASSIGN COMPONENT 'MSGV2' OF STRUCTURE <ls_in> TO <l_in>.
      IF sy-subrc EQ 0.
        ls_s_msg-msgv2 = <l_in>.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGV2' OF STRUCTURE <ls_in> TO <l_in>.
        IF sy-subrc EQ 0.
          ls_s_msg-msgv2 = <l_in>.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'MESSAGE_V3' OF STRUCTURE <ls_in> TO <l_in>.
    IF sy-subrc EQ 0.
      ls_s_msg-msgv3 = <l_in>.
    ELSE.
      ASSIGN COMPONENT 'MSGV3' OF STRUCTURE <ls_in> TO <l_in>.
      IF sy-subrc EQ 0.
        ls_s_msg-msgv3 = <l_in>.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGV3' OF STRUCTURE <ls_in> TO <l_in>.
        IF sy-subrc EQ 0.
          ls_s_msg-msgv3 = <l_in>.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'MESSAGE_V4' OF STRUCTURE <ls_in> TO <l_in>.
    IF sy-subrc EQ 0.
      ls_s_msg-msgv4 = <l_in>.
    ELSE.
      ASSIGN COMPONENT 'MSGV4' OF STRUCTURE <ls_in> TO <l_in>.
      IF sy-subrc EQ 0.
        ls_s_msg-msgv4 = <l_in>.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGV4' OF STRUCTURE <ls_in> TO <l_in>.
        IF sy-subrc EQ 0.
          ls_s_msg-msgv4 = <l_in>.
        ENDIF.
      ENDIF.
    ENDIF.

*write to destiny line
    ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_out> TO <l_out>.
    IF sy-subrc EQ 0.
      <l_out> = ls_s_msg-msgid.
    ELSE.
      ASSIGN COMPONENT 'MSGID' OF STRUCTURE <ls_out> TO <l_out>.
      IF sy-subrc EQ 0.
        <l_out> = ls_s_msg-msgid.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGID' OF STRUCTURE <ls_out> TO <l_out>.
        IF sy-subrc EQ 0.
          <l_out> = ls_s_msg-msgid.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_out> TO <l_out>.
    IF sy-subrc EQ 0.
      <l_out> = ls_s_msg-msgty.
    ELSE.
      ASSIGN COMPONENT 'MSGTYP' OF STRUCTURE <ls_out> TO <l_out>.
      IF sy-subrc EQ 0.
        <l_out> = ls_s_msg-msgty.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGTYP' OF STRUCTURE <ls_out> TO <l_out>.
        IF sy-subrc EQ 0.
          <l_out> = ls_s_msg-msgty.
        ELSE.
          ASSIGN COMPONENT 'MSGTY' OF STRUCTURE <ls_out> TO <l_out>.
          IF sy-subrc EQ 0.
            <l_out> = ls_s_msg-msgty.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'NUMBER' OF STRUCTURE <ls_out> TO <l_out>.
    IF sy-subrc EQ 0.
      <l_out> = ls_s_msg-msgno.
    ELSE.
      ASSIGN COMPONENT 'MSGNR' OF STRUCTURE <ls_out> TO <l_out>.
      IF sy-subrc EQ 0.
        <l_out> = ls_s_msg-msgno.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGNR' OF STRUCTURE <ls_out> TO <l_out>.
        IF sy-subrc EQ 0.
          <l_out> = ls_s_msg-msgno.
        ELSE.
          ASSIGN COMPONENT 'MSGNO' OF STRUCTURE <ls_out> TO <l_out>.
          IF sy-subrc EQ 0.
            <l_out> = ls_s_msg-msgno.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'MESSAGE_V1' OF STRUCTURE <ls_out> TO <l_out>.
    IF sy-subrc EQ 0.
      <l_out> = ls_s_msg-msgv1.
    ELSE.
      ASSIGN COMPONENT 'MSGV1' OF STRUCTURE <ls_out> TO <l_out>.
      IF sy-subrc EQ 0.
        <l_out> = ls_s_msg-msgv1.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGV1' OF STRUCTURE <ls_out> TO <l_out>.
        IF sy-subrc EQ 0.
          <l_out> = ls_s_msg-msgv1.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'MESSAGE_V2' OF STRUCTURE <ls_out> TO <l_out>.
    IF sy-subrc EQ 0.
      <l_out> = ls_s_msg-msgv2.
    ELSE.
      ASSIGN COMPONENT 'MSGV2' OF STRUCTURE <ls_out> TO <l_out>.
      IF sy-subrc EQ 0.
        <l_out> = ls_s_msg-msgv2.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGV2' OF STRUCTURE <ls_out> TO <l_out>.
        IF sy-subrc EQ 0.
          <l_out> = ls_s_msg-msgv2.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'MESSAGE_V3' OF STRUCTURE <ls_out> TO <l_out>.
    IF sy-subrc EQ 0.
      <l_out> = ls_s_msg-msgv3.
    ELSE.
      ASSIGN COMPONENT 'MSGV3' OF STRUCTURE <ls_out> TO <l_out>.
      IF sy-subrc EQ 0.
        <l_out> = ls_s_msg-msgv3.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGV3' OF STRUCTURE <ls_out> TO <l_out>.
        IF sy-subrc EQ 0.
          <l_out> = ls_s_msg-msgv3.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'MESSAGE_V4' OF STRUCTURE <ls_out> TO <l_out>.
    IF sy-subrc EQ 0.
      <l_out> = ls_s_msg-msgv4.
    ELSE.
      ASSIGN COMPONENT 'MSGV4' OF STRUCTURE <ls_out> TO <l_out>.
      IF sy-subrc EQ 0.
        <l_out> = ls_s_msg-msgv4.
      ELSE.
        ASSIGN COMPONENT 'ERR-MSGV4' OF STRUCTURE <ls_out> TO <l_out>.
        IF sy-subrc EQ 0.
          <l_out> = ls_s_msg-msgv4.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


method display_return_messages.
*ERS 2015-09-04 - should work for batch input, bapi and idoc return tables

* Message log structures
  data: ls_s_log  type bal_s_log,
        ls_s_msg  type bal_s_msg,
        ls_s_prof type bal_s_prof,
        l_handle  type balloghndl.

  ls_s_log-aldate = sy-datum.
  ls_s_log-altime = sy-uzeit.
  ls_s_log-aluser = sy-uname.
  ls_s_log-alprog = sy-cprog. "repid.


  call function 'BAL_LOG_CREATE'
    exporting
      i_s_log                 = ls_s_log
    importing
      e_log_handle            = l_handle
    exceptions
      log_header_inconsistent = 1
      others                  = 2.


  field-symbols: <ls> type any, <l> type any.

  loop at it_return assigning <ls>.
    clear ls_s_msg.

    assign component 'ID' of structure <ls> to <l>.
    if sy-subrc eq 0.
      ls_s_msg-msgid = <l>.
    else.
      assign component 'MSGID' of structure <ls> to <l>.
      if sy-subrc eq 0.
        ls_s_msg-msgid = <l>.
      else.
        assign component 'ERR-MSGID' of structure <ls> to <l>.
        if sy-subrc eq 0.
          ls_s_msg-msgid = <l>.
        endif.
      endif.
    endif.
    assign component 'TYPE' of structure <ls> to <l>.
    if sy-subrc eq 0.
      ls_s_msg-msgty = <l>.
    else.
      assign component 'MSGTYP' of structure <ls> to <l>.
      if sy-subrc eq 0.
        ls_s_msg-msgty = <l>.
      else.
        assign component 'ERR-MSGTYP' of structure <ls> to <l>.
        if sy-subrc eq 0.
          ls_s_msg-msgty = <l>.
        else.
          assign component 'MSGTY' of structure <ls> to <l>.
          if sy-subrc eq 0.
            ls_s_msg-msgty = <l>.
          endif.
        endif.
      endif.
    endif.
    assign component 'NUMBER' of structure <ls> to <l>.
    if sy-subrc eq 0.
      ls_s_msg-msgno = <l>.
    else.
      assign component 'MSGNR' of structure <ls> to <l>.
      if sy-subrc eq 0.
        ls_s_msg-msgno = <l>.
      else.
        assign component 'ERR-MSGNR' of structure <ls> to <l>.
        if sy-subrc eq 0.
          ls_s_msg-msgno = <l>.
        else.
          assign component 'MSGNO' of structure <ls> to <l>.
          if sy-subrc eq 0.
            ls_s_msg-msgno = <l>.
          endif.
        endif.
      endif.
    endif.
    assign component 'MESSAGE_V1' of structure <ls> to <l>.
    if sy-subrc eq 0.
      ls_s_msg-msgv1 = <l>.
    else.
      assign component 'MSGV1' of structure <ls> to <l>.
      if sy-subrc eq 0.
        ls_s_msg-msgv1 = <l>.
      else.
        assign component 'ERR-MSGV1' of structure <ls> to <l>.
        if sy-subrc eq 0.
          ls_s_msg-msgv1 = <l>.
        endif.
      endif.
    endif.
    assign component 'MESSAGE_V2' of structure <ls> to <l>.
    if sy-subrc eq 0.
      ls_s_msg-msgv2 = <l>.
    else.
      assign component 'MSGV2' of structure <ls> to <l>.
      if sy-subrc eq 0.
        ls_s_msg-msgv2 = <l>.
      else.
        assign component 'ERR-MSGV2' of structure <ls> to <l>.
        if sy-subrc eq 0.
          ls_s_msg-msgv2 = <l>.
        endif.
      endif.
    endif.
    assign component 'MESSAGE_V3' of structure <ls> to <l>.
    if sy-subrc eq 0.
      ls_s_msg-msgv3 = <l>.
    else.
      assign component 'MSGV3' of structure <ls> to <l>.
      if sy-subrc eq 0.
        ls_s_msg-msgv3 = <l>.
      else.
        assign component 'ERR-MSGV3' of structure <ls> to <l>.
        if sy-subrc eq 0.
          ls_s_msg-msgv3 = <l>.
        endif.
      endif.
    endif.
    assign component 'MESSAGE_V4' of structure <ls> to <l>.
    if sy-subrc eq 0.
      ls_s_msg-msgv4 = <l>.
    else.
      assign component 'MSGV4' of structure <ls> to <l>.
      if sy-subrc eq 0.
        ls_s_msg-msgv4 = <l>.
      else.
        assign component 'ERR-MSGV4' of structure <ls> to <l>.
        if sy-subrc eq 0.
          ls_s_msg-msgv4 = <l>.
        endif.
      endif.
    endif.

    case ls_s_msg-msgty.
      when 'E'.
        ls_s_msg-probclass = 1.
      when 'W'.
        ls_s_msg-probclass = 2.
      when 'I'.
        ls_s_msg-probclass = 3.
      when 'S'.
        ls_s_msg-probclass = 4.
    endcase.

    call function 'BAL_LOG_MSG_ADD'
      exporting
        i_log_handle     = l_handle
        i_s_msg          = ls_s_msg
      exceptions
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        others           = 4.


  endloop.

  call function 'BAL_DSP_PROFILE_POPUP_GET'
    importing
      e_s_display_profile = ls_s_prof.


  call function 'BAL_DSP_LOG_DISPLAY'
    exporting
      i_s_display_profile  = ls_s_prof
    exceptions
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      others               = 5.

  call function 'BAL_LOG_REFRESH'
    exporting
      i_log_handle  = l_handle
    exceptions
      log_not_found = 1
      others        = 2.

endmethod.


  METHOD execute.
    CALL TRANSACTION tcode USING bdcdata
                   MODE   mode
                   UPDATE update
                   MESSAGES INTO messtab.
  ENDMETHOD.


  METHOD refresh_bdc.
    CLEAR bdcdata.
  ENDMETHOD.
ENDCLASS.
