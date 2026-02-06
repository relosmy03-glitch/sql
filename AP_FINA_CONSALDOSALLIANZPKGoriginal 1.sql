create or replace PACKAGE BODY Ap_fina_ConSaldosAllianzPkg AS

dblSalIniLoc_g SPC___.SPC____SALINILOC_B%TYPE;  -- Saldo inicial en moneda local
dblSalIniOri_g SPC___.SPC____SALINILOC_B%TYPE;  -- Saldo inicial en moneda origen
dblDebiLoca__g SPC___.SPC____SALINILOC_B%TYPE;  -- Movimiento débito en moneda local
dblCredLoca__g SPC___.SPC____SALINILOC_B%TYPE;  -- Movimiento crédito en moneda local
dblDebiOrig__g SPC___.SPC____SALINILOC_B%TYPE;  -- Movimiento débito en moneda origen
dblCredOrig__g SPC___.SPC____SALINILOC_B%TYPE;  -- Movimiento crédito en moneda origen
dblSalFinLoc_g SPC___.SPC____SALINILOC_B%TYPE;  -- Saldo final en moneda local
dblSalFinOri_g SPC___.SPC____SALINILOC_B%TYPE;  -- Saldo final en moneda origen
chrIndHijSal_g VARCHAR2(1);                     -- Indicador de si las cuentas hijas tuvieron saldo
intFacNatCue_g NUMBER(1);                       -- Naturaleza de la cuenta

-- Variable que llena el procedimiento CalcularSaldoCuenta1 para optimizar el proceso
chrCodPlaCue_g SPC___.SPC____CODIGO____PC_____B%TYPE; -- Código plan de cuentas
chrNatuSald__g CPC___.CPC____NATUSALD__B%TYPE;        -- Naturaleza del saldo de la cuenta
chrCodPerFis_g PERIOD.PERIOD_CODIGO____PF_____B%TYPE; -- Código del período fiscal
intNumePeri__g PERIOD.PERIOD_NUMERO____B%TYPE;        -- Número del período
chrFecIniPer_g PERIOD.PERIOD_FECHINIC__B%TYPE;        -- Fecha inicial del período
chrFecFinPer_g PERIOD.PERIOD_FECHFINA__B%TYPE;        -- Fecha final del período

--Req. 6903 LVELASQUEZ Leonardo Velasquez
dblSalIniLo1_g NUMBER(20,2);             -- Saldo inicial en moneda local
dblSalIniOr1_g NUMBER(20,2);             -- Saldo inicial en moneda origen
dblDebiLoc1__g NUMBER(20,2);             -- Movimiento débito en moneda local
dblCredLoc1__g NUMBER(20,2);             -- Movimiento crédito en moneda local
dblDebiOri1__g NUMBER(20,2);             -- Movimiento débito en moneda origen
dblCredOri1__g NUMBER(20,2);             -- Movimiento crédito en moneda origen
dblSalFinLo1_g NUMBER(20,2);             -- Saldo final en moneda local
dblSalFinOr1_g NUMBER(20,2);             -- Saldo final en moneda origen

PROCEDURE AcumularSaldosCuenta(
intSalDebLoc_p IN SPC___.SPC____SALINILOC_B%TYPE,
intSalCreLoc_p IN SPC___.SPC____SALINILOC_B%TYPE,
intSalDebOri_p IN SPC___.SPC____SALINIORI_B%TYPE,
intSalCreOri_p IN SPC___.SPC____SALINIORI_B%TYPE,
intSalDebAju_p IN SPC___.SPC____SALINILOC_B%TYPE,
intSalCreAju_p IN SPC___.SPC____SALINILOC_B%TYPE,
intCieDebLoc_p IN SPC___.SPC____SALINILOC_B%TYPE,
intCieCreLoc_p IN SPC___.SPC____SALINILOC_B%TYPE,
intCieDebOri_p IN SPC___.SPC____SALINILOC_B%TYPE,
intCieCreOri_p IN SPC___.SPC____SALINILOC_B%TYPE,
chrInclCier__p IN VARCHAR2,
chrInclAjus__p IN VARCHAR2
) IS
BEGIN
  dblDebiLoca__g := dblDebiLoca__g + NVL(intSalDebLoc_p, 0);
  dblCredLoca__g := dblCredLoca__g + NVL(intSalCreLoc_p, 0);
  dblDebiOrig__g := dblDebiOrig__g + NVL(intSalDebOri_p, 0);
  dblCredOrig__g := dblCredOrig__g + NVL(intSalCreOri_p, 0);
  IF chrInclAjus__p='S' THEN
    dblDebiLoca__g := dblDebiLoca__g + NVL(intSalDebAju_p,0);
    dblCredLoca__g := dblCredLoca__g + NVL(intSalCreAju_p,0);
  END IF;

  -- Se acumulan los saldos de cierre
  IF (chrInclCier__p = 'S') THEN
    dblDebiLoca__g := dblDebiLoca__g + NVL(intCieDebLoc_p,0);
    dblCredLoca__g := dblCredLoca__g + NVL(intCieCreLoc_p,0);
    dblDebiOrig__g := dblDebiOrig__g + NVL(intCieDebOri_p,0);
    dblCredOrig__g := dblCredOrig__g + NVL(intCieCreOri_p,0);
  END IF;
END AcumularSaldosCuenta;

PROCEDURE Ap_fina_ConSaldos (
   chrCodiCont__p IN  MC____.MC_____CODIGO____CONTAB_B%TYPE,
   chrFecInicia_p IN  MC____.MC_____FECHA_____B%TYPE,
   chrFecFinal__p IN  MC____.MC_____FECHA_____B%TYPE,
   chrIncAjuste_p IN  MC____.MC_____INDMOVREV_B%TYPE,
   chrIncCierre_p IN  MC____.MC_____INDMOVREV_B%TYPE,
   chrConSaldos_p OUT arrConSaldos_p,
   intNumRegist_p OUT NUMBER
) IS

  i number;
 
  CURSOR curConsCuentas__t IS
   SELECT * FROM (
      SELECT CPC____CODIGO____B CUENTA, CPC____NOMBRE____B NOMBRE,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'IL') SALDO_INICIAL,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'DL') DEBITOS,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'CL') CREDITOS,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'FL') SALDO_FINAL
        FROM ( SELECT CPC____CODIGO____B, CPC____NOMBRE____B,
                      CONTSQL7.SaldosCuenta(CONTAB_CODIGO____B, CPC____CODIGO____B,
                      chrFecInicia_p, chrFecFinal__p,
                      chrIncAjuste_p, chrIncCierre_p, NULL, 'N', NULL) RESULTADO
                 FROM CONTAB, CPC___
                WHERE CONTAB_CODIGO____PC_____B = CPC____CODIGO____PC_____B
                  AND CONTAB_CODIGO____B = chrCodiCont__p
                  AND LENGTH(CPC____CODIGO____B) <= 10 )
             )
       WHERE (SALDO_INICIAL <> 0) 
          OR (DEBITOS <> 0) 
          OR (CREDITOS <> 0) 
          OR (SALDO_FINAL <> 0)
    ORDER BY CUENTA;
BEGIN
   i := 0;
   FOR regConsCuentas__t IN curConsCuentas__t LOOP
      chrConSaldos_p(i) := regConsCuentas__t.CUENTA || '|' || regConsCuentas__t.NOMBRE || '|' ||
                                    regConsCuentas__t.SALDO_INICIAL || '|' || regConsCuentas__t.DEBITOS || '|' ||
                                    regConsCuentas__t.CREDITOS || '|' || regConsCuentas__t.SALDO_FINAL || '@';
      i := i + 1;
   END LOOP;
END Ap_fina_ConSaldos;

PROCEDURE Ap_fina_ConSaldos2 (
   intSecuInte__p IN  SCALL_.SCALL__SECINTDOC_B%TYPE,
   chrCodiCont__p IN  MC____.MC_____CODIGO____CONTAB_B%TYPE,
   chrFecInicia_p IN  MC____.MC_____FECHA_____B%TYPE,
   chrFecFinal__p IN  MC____.MC_____FECHA_____B%TYPE,
   chrIncAjuste_p IN  MC____.MC_____INDMOVREV_B%TYPE,
   chrIncCierre_p IN  MC____.MC_____INDMOVREV_B%TYPE,
   chrIndValExi_p OUT VARCHAR2,                                                                        
   chrCadeErro__p OUT RFT_FD____.FD_____CADERRREG_B%TYPE 
) IS

  i number;
  intCursSele__t INTEGER;           -- ID del cursor dinamico
  intNumLinPro_t NUMBER(6);         -- Número de lineas procesadas
  pComillas VARCHAR2(4);
  chrTextSele__t VARCHAR2(20000);   --SQL dinámico
  chrCodiCont__t MC____.MC_____CODIGO____CONTAB_B%TYPE;
  chrFecSystem_t MC____.MC_____FECHA_____B%TYPE;
  CURSOR curConsCuentas__t IS
   SELECT * FROM (
      SELECT CPC____CODIGO____B CUENTA, CPC____NOMBRE____B NOMBRE,
             CPC____CODIGO____MONEDA_B MONEDA,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'IL') SALDO_INICIAL,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'DL') DEBITOS,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'CL') CREDITOS,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'FL') SALDO_FINAL
        FROM ( SELECT CPC____CODIGO____B, CPC____NOMBRE____B,
                      CPC____CODIGO____MONEDA_B,
                      --CONTSQL7.SaldosCuenta(CONTAB_CODIGO____B, CPC____CODIGO____B,
                      SaldosCuenta(CONTAB_CODIGO____B, CPC____CODIGO____B,
                      chrFecInicia_p, chrFecFinal__p,
                      chrIncAjuste_p, chrIncCierre_p, NULL, 'N', NULL) RESULTADO
                 FROM CONTAB, CPC___
                WHERE CONTAB_CODIGO____PC_____B = CPC____CODIGO____PC_____B
                  AND CONTAB_CODIGO____B = chrCodiCont__p
                  AND LENGTH(CPC____CODIGO____B) <= 10 )
             )
       WHERE (SALDO_INICIAL <> 0) 
          OR (DEBITOS <> 0) 
          OR (CREDITOS <> 0) 
          OR (SALDO_FINAL <> 0)
    ORDER BY CUENTA;
    
  CURSOR curConsCuentaI__t IS
   SELECT * FROM (
      SELECT CPC____CODIGO____B CUENTA, CPC____NOMBRE____B NOMBRE,
             CPC____CODIGO____MONEDA_B MONEDA,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'IL') SALDO_INICIAL,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'DL') DEBITOS,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'CL') CREDITOS,
             CONTSQL7.ObtenerSaldo(RESULTADO, 'FL') SALDO_FINAL
        FROM ( SELECT CPC____CODIGO____B, CPC____NOMBRE____B,
                      CPC____CODIGO____MONEDA_B,
                      CONTSQL7.SaldosCuenta(CONTAB_CODIGO____B, CPC____CODIGO____B,
                      --SaldosCuenta(CONTAB_CODIGO____B, CPC____CODIGO____B,
                      chrFecInicia_p, chrFecFinal__p,
                      chrIncAjuste_p, chrIncCierre_p, NULL, 'N', NULL) RESULTADO
                 FROM CONTAB, CPC___
                WHERE CONTAB_CODIGO____PC_____B = CPC____CODIGO____PC_____B
                  AND CONTAB_CODIGO____B = chrCodiCont__p
                  AND LENGTH(CPC____CODIGO____B) <= 10 )
             )
       WHERE (SALDO_INICIAL <> 0) 
          OR (DEBITOS <> 0) 
          OR (CREDITOS <> 0) 
          OR (SALDO_FINAL <> 0)
    ORDER BY CUENTA;    
BEGIN
     TRAZAFOGP('seguimiento 1');   
   pComillas := '''';
   chrCadeErro__p := '';
   chrIndValExi_p := 'S';
   
   BEGIN
      SELECT CONTAB_CODIGO____B
        INTO chrCodiCont__t
        FROM CONTAB
       WHERE CONTAB_CODIGO____B = chrCodiCont__p;
    EXCEPTION
       WHEN NO_DATA_FOUND THEN
          chrIndValExi_p := 'N';
          chrCadeErro__p := chr(13) || chrcadeerro__p ||' ERROR. La contabilidad [' || chrcodicont__p || '] NO EXISTE';
   END;
   TRAZAFOGP('seguimiento 2');
   BEGIN
      SELECT PARAME_VALOR_____B
        INTO chrCodiCont__t
        FROM PARAME
       WHERE PARAME_CODIGO____SISTEM_B = 'FINA'
         AND PARAME_NUMERO____B = 91;
    EXCEPTION
       WHEN NO_DATA_FOUND THEN
          chrIndValExi_p := 'N';
          chrCadeErro__p := chr(13) || chrcadeerro__p ||' ERROR. Parámetro para contabilidad por defecto NO EXISTE';
       WHEN OTHERS THEN
         TRAZAFOGP(SQLERRM);
   END;
   TRAZAFOGP('seguimiento 3');
   IF( chrFecInicia_p > chrFecFinal__p ) THEN
      chrIndValExi_p := 'N';
      chrCadeErro__p := chr(13) || chrcadeerro__p ||
                        ' ERROR. La fecha inicial es superior a la fecha final';
   END IF;
   TRAZAFOGP('seguimiento 4');
   SELECT SYSDATE
     INTO chrFecSystem_t
     FROM DUAL;
   IF( chrFecInicia_p > chrFecSystem_t OR chrFecFinal__p > chrFecSystem_t ) THEN
      chrIndValExi_p := 'N';
      chrCadeErro__p := chr(13) || chrcadeerro__p ||
                        ' ERROR. Favor verificar que las fechas ingresadas no superen la fecha del sistema';
   END IF;
   TRAZAFOGP('seguimiento 5');
   IF( chrIndValExi_p = 'S' ) THEN
      IF( NVL(intSecuInte__p, -999) <= 0 ) THEN
         chrIndValExi_p := 'N';
         chrCadeErro__p := chr(13) || chrcadeerro__p || 'CONSECUTIVO NO VÁLIDO';
      ELSE
         --intCursSele__t := DBMS_SQL.OPEN_CURSOR;
         IF( chrCodiCont__t = chrCodiCont__p ) THEN
            FOR regConsCuentas__t IN curConsCuentas__t LOOP
               chrTextSele__t := 'INSERT INTO SCALL_ ( SCALL__SECINTDOC_B, '||
                                                      ' SCALL__CODIGO____CPC____B, SCALL__CPC____NOMBRE____B,  '||
                                                      ' SCALL__MONEDA_CODIGO____B, '||
                                                      ' SCALL__SALINILOC_B, SCALL__ACUDEBLOC_B, '||
                                                      ' SCALL__ACUCRELOC_B, SCALL__SALFINLOC_B, '||
                                                      ' SCALL__FECHORCRE_B, SCALL__AUTOCREA__B )'||
                                             ' VALUES (' || intSecuInte__p || ', ' ||
                                             pComillas || regConsCuentas__t.CUENTA || pComillas || ', ' || 
                                             pComillas || regConsCuentas__t.NOMBRE || pComillas || ', ' ||
                                             pComillas || regConsCuentas__t.MONEDA || pComillas || ', ' ||
                                             pComillas || regConsCuentas__t.SALDO_INICIAL || pComillas || ', ' || 
                                             pComillas || regConsCuentas__t.DEBITOS || pComillas || ', ' ||
                                             pComillas || regConsCuentas__t.CREDITOS || pComillas || ', ' || 
                                             pComillas || regConsCuentas__t.SALDO_FINAL || pComillas || ', ' ||
                                             'SYSDATE, USER )';
    
               
               BEGIN
                  --DBMS_SQL.PARSE(intCursSele__t,chrTextSele__t,DBMS_SQL.NATIVE);
                  EXECUTE IMMEDIATE(chrTextSele__t);
               EXCEPTION
                  WHEN OTHERS THEN
                     --DBMS_SQL.CLOSE_CURSOR (intCursSele__t);
                     chrIndValExi_p := 'N';
                     chrCadeErro__p := chr(13) || chrcadeerro__p ||'SQL ERROR: ' || chrTextSele__t;
               END;
         
               --intNumLinPro_t := DBMS_SQL.EXECUTE(intCursSele__t);
            END LOOP;
            --DBMS_SQL.CLOSE_CURSOR(intCursSele__t);
         ELSE
            FOR regConsCuentaI__t IN curConsCuentaI__t LOOP
               chrTextSele__t := 'INSERT INTO SCALL_ ( SCALL__SECINTDOC_B, '||
                                                      ' SCALL__CODIGO____CPC____B, SCALL__CPC____NOMBRE____B,  '||
                                                      ' SCALL__MONEDA_CODIGO____B, '||
                                                      ' SCALL__SALINILOC_B, SCALL__ACUDEBLOC_B, '||
                                                      ' SCALL__ACUCRELOC_B, SCALL__SALFINLOC_B, '||
                                                      ' SCALL__FECHORCRE_B, SCALL__AUTOCREA__B )'||
                                             ' VALUES (' || intSecuInte__p || ', ' ||
                                             pComillas || regConsCuentaI__t.CUENTA || pComillas || ', ' || 
                                             pComillas || regConsCuentaI__t.NOMBRE || pComillas || ', ' ||
                                             pComillas || regConsCuentaI__t.MONEDA || pComillas || ', ' ||
                                             pComillas || regConsCuentaI__t.SALDO_INICIAL || pComillas || ', ' || 
                                             pComillas || regConsCuentaI__t.DEBITOS || pComillas || ', ' ||
                                             pComillas || regConsCuentaI__t.CREDITOS || pComillas || ', ' || 
                                             pComillas || regConsCuentaI__t.SALDO_FINAL || pComillas || ', ' ||
                                             'SYSDATE, USER )';
    
               
               BEGIN
                  --DBMS_SQL.PARSE(intCursSele__t,chrTextSele__t,DBMS_SQL.NATIVE);
                  EXECUTE IMMEDIATE(chrTextSele__t);
               EXCEPTION
                  WHEN OTHERS THEN
                     --DBMS_SQL.CLOSE_CURSOR (intCursSele__t);
                     chrIndValExi_p := 'N';
                     chrCadeErro__p := chr(13) || chrcadeerro__p ||'SQL ERROR: ' || chrTextSele__t;
               END;
         
               --intNumLinPro_t := DBMS_SQL.EXECUTE(intCursSele__t);
            END LOOP;
      END IF;  
      END IF;
   END IF;
   TRAZAFOGP('seguimiento 6 '||chrCadeErro__p);
END Ap_fina_ConSaldos2;

PROCEDURE RegistrarProceso( 
intSecuInte__p OUT SCALL_.SCALL__SECINTDOC_B%TYPE,
chrIndValExi_p OUT VARCHAR2,
chrCadeErro__p OUT RFT_FD____.FD_____CADERRREG_B%TYPE
) IS

intSecuInte__t SCALL_.SCALL__SECINTDOC_B%TYPE;
chrIndValExi_t VARCHAR2(1);
chrCadeErro__t RFT_FD____.FD_____CADERRREG_B%TYPE;

BEGIN 

-- Inicializar las variables
   intSecuInte__t := NULL; 
   chrIndValExi_t := 'S';
   chrCadeErro__t := NULL; 
   BEGIN 
      SELECT SCALL__SEQ.NEXTVAL CONSECUTIVO
        INTO intSecuInte__t
        FROM DUAL;
       EXCEPTION 
         WHEN OTHERS THEN 
            chrCadeErro__t := 'Error consultado consecutivo ' || SUBSTR(SQLERRM, 1, 200); 
            chrIndValExi_t := 'N';
   END;

   intSecuInte__p := intSecuInte__t; 
   chrIndValExi_p := chrIndValExi_t; 
   chrCadeErro__p := chrCadeErro__t; 
END RegistrarProceso; 

PROCEDURE EliminarProceso( 
intSecuInte__p IN  SCALL_.SCALL__SECINTDOC_B%TYPE,
chrIndValExi_p OUT VARCHAR2,
chrCadeErro__p OUT RFT_FD____.FD_____CADERRREG_B%TYPE
) IS

intSecuInte__t SCALL_.SCALL__SECINTDOC_B%TYPE;
chrIndValExi_t VARCHAR2(1);
chrCadeErro__t RFT_FD____.FD_____CADERRREG_B%TYPE;

BEGIN 
  trazafogp('seguimiento 7');
-- Inicializar las variables
   chrIndValExi_t := 'S';
   chrCadeErro__t := NULL; 
   BEGIN 
      DELETE FROM SCALL_ 
       WHERE SCALL__SECINTDOC_B = intSecuInte__p;
       EXCEPTION 
         WHEN OTHERS THEN 
            chrCadeErro__t := 'Error eliminado en SCALL_ ' || SUBSTR(SQLERRM, 1, 200); 
            chrIndValExi_t := 'N';
   END;
   trazafogp('seguimiento 8'||chrCadeErro__t);
   chrIndValExi_p := chrIndValExi_t; 
   chrCadeErro__p := chrCadeErro__t; 
    trazafogp('seguimiento 9'||chrCadeErro__t);
END EliminarProceso; 


FUNCTION SaldosCuenta(
chrCodiCont__p IN SPC___.SPC____CODIGO____CONTAB_B%TYPE, -- Código de contabilidad
chrCodiCuen__p IN SPC___.SPC____CODIGO____CPC____B%TYPE, -- Cuenta
chrFechInic__p IN DATE,                                  -- Fecha inicial 
chrFechFina__p IN DATE,                                  -- Fecha final 
chrInclAjus__p IN VARCHAR2,                              -- Indicador de si incluye ajustes
chrInclCier__p IN VARCHAR2,                              -- Indicador de si incluye cierres
chrCodCenUti_p IN SPCCU_.SPCCU__CODIGO____CU_____B%TYPE, -- Código de centro de utilidad
chrIncArbCen_p IN VARCHAR2,                              -- Indicador de si incluye arbol de centros
intIdenTerc__p IN SPCT__.SPCT___IDENTIFIC_TERCER_B%TYPE  -- Identificación del tercero
) RETURN VARCHAR2 IS

dblSILoFeIn__t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo inicial en moneda local para la fecha inicial 
dblSIOrFeIn__t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo inicial en moneda origen para la fecha inicial 
dblDeLoFeIn__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento débito en moneda local para la fecha inicial 
dblCrLoFeIn__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento crédito en moneda local para la fecha inicial 
dblDeOrFeIn__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento débito en moneda origen para la fecha inicial 
dblCrOrFeIn__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento crédito en moneda origen para la fecha inicial 
dblSFLoFeIn__t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo final en moneda local para la fecha inicial 
dblSFOrFeIn__t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo final en moneda origen para la fecha inicial 

dblSILoFeFi__t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo inicial en moneda local para la fecha final 
dblSIOrFeFi__t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo inicial en moneda origen para la fecha final 
dblDeLoFeFi__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento débito en moneda local para la fecha final 
dblCrLoFeFi__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento crédito en moneda local para la fecha final 
dblDeOrFeFi__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento débito en moneda origen para la fecha final 
dblCrOrFeFi__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento crédito en moneda origen para la fecha final 
dblSFLoFeFi__t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo final en moneda local para la fecha final 
dblSFOrFeFi__t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo final en moneda origen para la fecha final 

dblSalIniLoc_t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo inicial en moneda local
dblSalIniOri_t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo inicial en moneda origen
dblDebiLoca__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento débito en moneda local
dblCredLoca__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento crédito en moneda local
dblDebiOrig__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento débito en moneda origen
dblCredOrig__t SPC___.SPC____SALINILOC_B%TYPE;       -- Movimiento crédito en moneda origen
dblSalFinLoc_t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo final en moneda local
dblSalFinOri_t SPC___.SPC____SALINILOC_B%TYPE;       -- Saldo final en moneda origen
chrIndHijSal_t VARCHAR2(1);

intNumDiaFec_t NUMBER(2);
intNumMesFec_t NUMBER(2);
intNumAnoFec_t NUMBER(4);
datDiaAntFI__t DATE;

chrResultado_t VARCHAR2(400);

BEGIN

  --
  -- Calcula el saldo para la fecha final 
  -- 
  CalcularSaldoCuenta(chrCodiCont__p,  -- Código de contabilidad
                      chrCodiCuen__p,  -- Cuenta
                      chrFechFina__p,  -- Fecha
                      chrInclAjus__p,  -- Indicador de si incluye ajustes
                      chrInclCier__p,  -- Indicador de si incluye cierres
                      chrCodCenUti_p,  -- Código de centro de utilidad
                      chrIncArbCen_p,  -- Indicador de si incluye arbol de centros
                      intIdenTerc__p,  -- Identificación del tercero
                      dblSILoFeFi__t,  -- Saldo inicial en moneda local
                      dblSIOrFeFi__t,  -- Saldo inicial en moneda origen
                      dblDeLoFeFi__t,  -- Movimiento débito en moneda local
                      dblCrLoFeFi__t,  -- Movimiento crédito en moneda local
                      dblDeOrFeFi__t,  -- Movimiento débito en moneda origen
                      dblCrOrFeFi__t,  -- Movimiento crédito en moneda origen
                      dblSFLoFeFi__t,  -- Saldo final en moneda local
                      dblSFOrFeFi__t,  -- Saldo final en moneda origen
                      chrIndHijSal_t);

  intNumDiaFec_t := EXTRACT(DAY FROM chrFechInic__p);
  intNumMesFec_t := EXTRACT(MONTH FROM chrFechInic__p);
  intNumAnoFec_t := EXTRACT(YEAR FROM chrFechInic__p);
  IF intNumDiaFec_t = 1 AND intNumMesFec_t = 1 THEN
    -- La fecha inicial es el primer dáa del aáo, los valores que se devuelven
    -- son los calculados para la fecha final  
    dblSalIniLoc_t := dblSILoFeFi__t;
    dblSalIniOri_t := dblSIOrFeFi__t;
    dblDebiLoca__t := dblDeLoFeFi__t;
    dblCredLoca__t := dblCrLoFeFi__t;
    dblDebiOrig__t := dblDeOrFeFi__t;
    dblCredOrig__t := dblCrOrFeFi__t;
    dblSalFinLoc_t := dblSFLoFeFi__t;
    dblSalFinOri_t := dblSFOrFeFi__t;

  ELSE
    -- La fecha inicial no es el primer dáa del aáo, se deben calcular los valores 
    -- para el dáa anterior a esta fecha y hacer las operaciones correspondientes 

    datDiaAntFI__t := (chrFechInic__p - 1);

    --
    -- Calcula el saldo para el dáa anterior a la la fecha inicial 
    -- 
    CalcularSaldoCuenta(chrCodiCont__p,  -- Código de contabilidad
                        chrCodiCuen__p,  -- Cuenta
                        datDiaAntFI__t,  -- Fecha
                        chrInclAjus__p,  -- Indicador de si incluye ajustes
                        chrInclCier__p,  -- Indicador de si incluye cierres
                        chrCodCenUti_p,  -- Código de centro de utilidad
                        chrIncArbCen_p,  -- Indicador de si incluye arbol de centros
                        intIdenTerc__p,  -- Identificación del tercero
                        dblSILoFeIn__t,  -- Saldo inicial en moneda local
                        dblSIOrFeIn__t,  -- Saldo inicial en moneda origen
                        dblDeLoFeIn__t,  -- Movimiento débito en moneda local
                        dblCrLoFeIn__t,  -- Movimiento crédito en moneda local
                        dblDeOrFeIn__t,  -- Movimiento débito en moneda origen
                        dblCrOrFeIn__t,  -- Movimiento crédito en moneda origen
                        dblSFLoFeIn__t,  -- Saldo final en moneda local
                        dblSFOrFeIn__t,  -- Saldo final en moneda origen
                        chrIndHijSal_t);

    -- El saldo inicial es el saldo final para el dáa anterior a la fecha inicial 
    dblSalIniLoc_t := dblSFLoFeIn__t;
    dblSalIniOri_t := dblSFOrFeIn__t;
    -- Los débitos y los créditos es la resta de los valores a la fecha final - menos los valores a la fecha inicial 
    dblDebiLoca__t := dblDeLoFeFi__t - dblDeLoFeIn__t;
    dblCredLoca__t := dblCrLoFeFi__t - dblCrLoFeIn__t;
    dblDebiOrig__t := dblDeOrFeFi__t - dblDeOrFeIn__t;
    dblCredOrig__t := dblCrOrFeFi__t - dblCrOrFeIn__t;
    -- El saldo final es el saldo final a la fecha final 
    dblSalFinLoc_t := dblSFLoFeFi__t;
    dblSalFinOri_t := dblSFOrFeFi__t;
  END IF;

  chrResultado_t := '<IL>' || dblSalIniLoc_t || '</IL>' ||
                    '<DL>' || dblDebiLoca__t || '</DL>' ||
                    '<CL>' || dblCredLoca__t || '</CL>' ||
                    '<FL>' || dblSalFinLoc_t || '</FL>' ||
                    '<IO>' || dblSalIniOri_t || '</IO>' ||
                    '<DO>' || dblDebiOrig__t || '</DO>' ||
                    '<CO>' || dblCredOrig__t || '</CO>' ||
                    '<FO>' || dblSalFinOri_t || '</FO>';

  RETURN(chrResultado_t);

END SaldosCuenta;

PROCEDURE CalcularSaldoCuenta(
chrCodiCont__p IN SPC___.SPC____CODIGO____CONTAB_B%TYPE, -- Código de contabilidad
chrCodiCuen__p IN SPC___.SPC____CODIGO____CPC____B%TYPE, -- Cuenta
chrFechSald__p IN DATE,                                  -- Fecha
chrInclAjus__p IN VARCHAR2,                              -- Indicador de si incluye ajustes
chrInclCier__p IN VARCHAR2,                              -- Indicador de si incluye cierres
chrCodCenUti_p IN SPCCU_.SPCCU__CODIGO____CU_____B%TYPE, -- Código de centro de utilidad
chrIncArbCen_p IN VARCHAR2,                              -- Indicador de si incluye arbol de centros
intIdenTerc__p IN SPCT__.SPCT___IDENTIFIC_TERCER_B%TYPE, -- Identificación del tercero
dblSalIniLoc_p OUT SPC___.SPC____SALINILOC_B%TYPE,       -- Saldo inicial en moneda local
dblSalIniOri_p OUT SPC___.SPC____SALINILOC_B%TYPE,       -- Saldo inicial en moneda origen
dblDebiLoca__p OUT SPC___.SPC____SALINILOC_B%TYPE,       -- Movimiento débito en moneda local
dblCredLoca__p OUT SPC___.SPC____SALINILOC_B%TYPE,       -- Movimiento crédito en moneda local
dblDebiOrig__p OUT SPC___.SPC____SALINILOC_B%TYPE,       -- Movimiento débito en moneda origen
dblCredOrig__p OUT SPC___.SPC____SALINILOC_B%TYPE,       -- Movimiento crédito en moneda origen
dblSalFinLoc_p OUT SPC___.SPC____SALINILOC_B%TYPE,       -- Saldo final en moneda local
dblSalFinOri_p OUT SPC___.SPC____SALINILOC_B%TYPE,       -- Saldo final en moneda origen
chrIndHijSal_p OUT VARCHAR2
) IS

chrIndBusMC__t VARCHAR2(1);  -- Indica si se debe buscar movimiento en MC___

chrCodPlaBas_t PC____.PC_____CODIGO____B%TYPE;
chrCodPlaEsp_t PC____.PC_____CODIGO____B%TYPE;

chrCodPerFis_t PERIOD.PERIOD_CODIGO____PF_____B%TYPE;  -- Período fiscal de la fecha especificada
intNumePeri__t PERIOD.PERIOD_NUMERO____B%TYPE;         -- Período de la fecha especificada
chrFecIniPer_t PERIOD.PERIOD_FECHINIC__B%TYPE;         -- Fecha inicial del período de la fecha especificada
chrFecFinPer_t PERIOD.PERIOD_FECHFINA__B%TYPE;         -- Fecha final del período de la fecha especificada
intUltPerSal_t PERIOD.PERIOD_NUMERO____B%TYPE;         -- Último período a tomar de saldos

intNatuCuen__t NUMBER(1);  -- Naturaleza de la cuenta
chrCodCenIni_t CU____.CU_____CODIGO____B%TYPE;
chrCodCenFin_t CU____.CU_____CODIGO____B%TYPE;

-- Cursor para recuperar los saldos por cuenta
CURSOR curSaldCuen__t IS
SELECT SPC____CODIGO____CONTAB_B,
       SPC____CODIGO____PC_____B,
       SPC____CODIGO____CPC____B,
       'xxxxxxxxxxxxxxxx' SPC____CODIGO____CU_____B,
        1234567890123456  SPC____IDENTIFIC_TERCER_B,
       SPC____CODIGO____PF_____B,
       DECODE(CPC____NATUSALD__B, ConsDdls.chrDebito____c, 1, ConsDdls.chrCredito___c, -1)        CPC____NATUSALD__B,
       SPC____SALINILOC_B,
       SPC____ACDELO01__B,
       SPC____ACCRLO01__B,
       SPC____ACDELO02__B,
       SPC____ACCRLO02__B,
       SPC____ACDELO03__B,
       SPC____ACCRLO03__B,
       SPC____ACDELO04__B,
       SPC____ACCRLO04__B,
       SPC____ACDELO05__B,
       SPC____ACCRLO05__B,
       SPC____ACDELO06__B,
       SPC____ACCRLO06__B,
       SPC____ACDELO07__B,
       SPC____ACCRLO07__B,
       SPC____ACDELO08__B,
       SPC____ACCRLO08__B,
       SPC____ACDELO09__B,
       SPC____ACCRLO09__B,
       SPC____ACDELO10__B,
       SPC____ACCRLO10__B,
       SPC____ACDELO11__B,
       SPC____ACCRLO11__B,
       SPC____ACDELO12__B,
       SPC____ACCRLO12__B,
       SPC____SALINIORI_B,
       SPC____ACDEOR01__B,
       SPC____ACCROR01__B,
       SPC____ACDEOR02__B,
       SPC____ACCROR02__B,
       SPC____ACDEOR03__B,
       SPC____ACCROR03__B,
       SPC____ACDEOR04__B,
       SPC____ACCROR04__B,
       SPC____ACDEOR05__B,
       SPC____ACCROR05__B,
       SPC____ACDEOR06__B,
       SPC____ACCROR06__B,
       SPC____ACDEOR07__B,
       SPC____ACCROR07__B,
       SPC____ACDEOR08__B,
       SPC____ACCROR08__B,
       SPC____ACDEOR09__B,
       SPC____ACCROR09__B,
       SPC____ACDEOR10__B,
       SPC____ACCROR10__B,
       SPC____ACDEOR11__B,
       SPC____ACCROR11__B,
       SPC____ACDEOR12__B,
       SPC____ACCROR12__B,
       CAC____CIDELO01__B,
       CAC____CIDELO02__B,
       CAC____CIDELO03__B,
       CAC____CIDELO04__B,
       CAC____CIDELO05__B,
       CAC____CIDELO06__B,
       CAC____CIDELO07__B,
       CAC____CIDELO08__B,
       CAC____CIDELO09__B,
       CAC____CIDELO10__B,
       CAC____CIDELO11__B,
       CAC____CIDELO12__B,
       CAC____CICRLO01__B,
       CAC____CICRLO02__B,
       CAC____CICRLO03__B,
       CAC____CICRLO04__B,
       CAC____CICRLO05__B,
       CAC____CICRLO06__B,
       CAC____CICRLO07__B,
       CAC____CICRLO08__B,
       CAC____CICRLO09__B,
       CAC____CICRLO10__B,
       CAC____CICRLO11__B,
       CAC____CICRLO12__B,
       CAC____CIDEOR01__B,
       CAC____CIDEOR02__B,
       CAC____CIDEOR03__B,
       CAC____CIDEOR04__B,
       CAC____CIDEOR05__B,
       CAC____CIDEOR06__B,
       CAC____CIDEOR07__B,
       CAC____CIDEOR08__B,
       CAC____CIDEOR09__B,
       CAC____CIDEOR10__B,
       CAC____CIDEOR11__B,
       CAC____CIDEOR12__B,
       CAC____CICROR01__B,
       CAC____CICROR02__B,
       CAC____CICROR03__B,
       CAC____CICROR04__B,
       CAC____CICROR05__B,
       CAC____CICROR06__B,
       CAC____CICROR07__B,
       CAC____CICROR08__B,
       CAC____CICROR09__B,
       CAC____CICROR10__B,
       CAC____CICROR11__B,
       CAC____CICROR12__B,
       CAC____AJUDEB01__B,
       CAC____AJUCRE01__B,
       CAC____AJUDEB02__B,
       CAC____AJUCRE02__B,
       CAC____AJUDEB03__B,
       CAC____AJUCRE03__B,
       CAC____AJUDEB04__B,
       CAC____AJUCRE04__B,
       CAC____AJUDEB05__B,
       CAC____AJUCRE05__B,
       CAC____AJUDEB06__B,
       CAC____AJUCRE06__B,
       CAC____AJUDEB07__B,
       CAC____AJUCRE07__B,
       CAC____AJUDEB08__B,
       CAC____AJUCRE08__B,
       CAC____AJUDEB09__B,
       CAC____AJUCRE09__B,
       CAC____AJUDEB10__B,
       CAC____AJUCRE10__B,
       CAC____AJUDEB11__B,
       CAC____AJUCRE11__B,
       CAC____AJUDEB12__B,
       CAC____AJUCRE12__B
FROM SPC___, CAC___, CPC___
WHERE SPC____CODIGO____CONTAB_B = CAC____CODIGO____CONTAB_B
  AND SPC____CODIGO____PC_____B = CAC____CODIGO____PC_____B
  AND SPC____CODIGO____CPC____B = CAC____CODIGO____CPC____B
  AND SPC____CODIGO____PF_____B = CAC____CODIGO____PF_____B
  AND SPC____CODIGO____PC_____B = CPC____CODIGO____PC_____B
  AND SPC____CODIGO____CPC____B = CPC____CODIGO____B
  --AND SPC____CODIGO____CONTAB_B = chrCodiCont__p
  AND SPC____CODIGO____PF_____B = chrCodPerFis_t
  AND (CPC____CODIGO____CONTAB_B IS NULL OR CPC____CODIGO____CONTAB_B = chrCodiCont__p)
  AND SPC____CODIGO____CPC____B BETWEEN chrCodiCuen__p AND RPAD(chrCodiCuen__p, 26, 'Z')
ORDER BY SPC____CODIGO____CPC____B;

-- Cursor para recuperar los saldos por cuenta - centro
CURSOR curSalCueCen_t IS
SELECT SPCCU__CODIGO____CONTAB_B SPC____CODIGO____CONTAB_B,
       SPCCU__CODIGO____PC_____B SPC____CODIGO____PC_____B,
       SPCCU__CODIGO____CPC____B SPC____CODIGO____CPC____B,
       SPCCU__CODIGO____CU_____B SPC____CODIGO____CU_____B,
       1234567890123456 SPC____IDENTIFIC_TERCER_B,
       SPCCU__CODIGO____PF_____B SPC____CODIGO____PF_____B,
       DECODE(CPC____NATUSALD__B, ConsDdls.chrDebito____c, 1, ConsDdls.chrCredito___c, -1)        CPC____NATUSALD__B,
       SPCCU__SALINILOC_B SPC____SALINILOC_B,
       SPCCU__ACDELO01__B SPC____ACDELO01__B,
       SPCCU__ACCRLO01__B SPC____ACCRLO01__B,
       SPCCU__ACDELO02__B SPC____ACDELO02__B,
       SPCCU__ACCRLO02__B SPC____ACCRLO02__B,
       SPCCU__ACDELO03__B SPC____ACDELO03__B,
       SPCCU__ACCRLO03__B SPC____ACCRLO03__B,
       SPCCU__ACDELO04__B SPC____ACDELO04__B,
       SPCCU__ACCRLO04__B SPC____ACCRLO04__B,
       SPCCU__ACDELO05__B SPC____ACDELO05__B,
       SPCCU__ACCRLO05__B SPC____ACCRLO05__B,
       SPCCU__ACDELO06__B SPC____ACDELO06__B,
       SPCCU__ACCRLO06__B SPC____ACCRLO06__B,
       SPCCU__ACDELO07__B SPC____ACDELO07__B,
       SPCCU__ACCRLO07__B SPC____ACCRLO07__B,
       SPCCU__ACDELO08__B SPC____ACDELO08__B,
       SPCCU__ACCRLO08__B SPC____ACCRLO08__B,
       SPCCU__ACDELO09__B SPC____ACDELO09__B,
       SPCCU__ACCRLO09__B SPC____ACCRLO09__B,
       SPCCU__ACDELO10__B SPC____ACDELO10__B,
       SPCCU__ACCRLO10__B SPC____ACCRLO10__B,
       SPCCU__ACDELO11__B SPC____ACDELO11__B,
       SPCCU__ACCRLO11__B SPC____ACCRLO11__B,
       SPCCU__ACDELO12__B SPC____ACDELO12__B,
       SPCCU__ACCRLO12__B SPC____ACCRLO12__B,
       SPCCU__SALINIORI_B SPC____SALINIORI_B,
       SPCCU__ACDEOR01__B SPC____ACDEOR01__B,
       SPCCU__ACCROR01__B SPC____ACCROR01__B,
       SPCCU__ACDEOR02__B SPC____ACDEOR02__B,
       SPCCU__ACCROR02__B SPC____ACCROR02__B,
       SPCCU__ACDEOR03__B SPC____ACDEOR03__B,
       SPCCU__ACCROR03__B SPC____ACCROR03__B,
       SPCCU__ACDEOR04__B SPC____ACDEOR04__B,
       SPCCU__ACCROR04__B SPC____ACCROR04__B,
       SPCCU__ACDEOR05__B SPC____ACDEOR05__B,
       SPCCU__ACCROR05__B SPC____ACCROR05__B,
       SPCCU__ACDEOR06__B SPC____ACDEOR06__B,
       SPCCU__ACCROR06__B SPC____ACCROR06__B,
       SPCCU__ACDEOR07__B SPC____ACDEOR07__B,
       SPCCU__ACCROR07__B SPC____ACCROR07__B,
       SPCCU__ACDEOR08__B SPC____ACDEOR08__B,
       SPCCU__ACCROR08__B SPC____ACCROR08__B,
       SPCCU__ACDEOR09__B SPC____ACDEOR09__B,
       SPCCU__ACCROR09__B SPC____ACCROR09__B,
       SPCCU__ACDEOR10__B SPC____ACDEOR10__B,
       SPCCU__ACCROR10__B SPC____ACCROR10__B,
       SPCCU__ACDEOR11__B SPC____ACDEOR11__B,
       SPCCU__ACCROR11__B SPC____ACCROR11__B,
       SPCCU__ACDEOR12__B SPC____ACDEOR12__B,
       SPCCU__ACCROR12__B SPC____ACCROR12__B,
       CACCU__CIDELO01__B CAC____CIDELO01__B,
       CACCU__CIDELO02__B CAC____CIDELO02__B,
       CACCU__CIDELO03__B CAC____CIDELO03__B,
       CACCU__CIDELO04__B CAC____CIDELO04__B,
       CACCU__CIDELO05__B CAC____CIDELO05__B,
       CACCU__CIDELO06__B CAC____CIDELO06__B,
       CACCU__CIDELO07__B CAC____CIDELO07__B,
       CACCU__CIDELO08__B CAC____CIDELO08__B,
       CACCU__CIDELO09__B CAC____CIDELO09__B,
       CACCU__CIDELO10__B CAC____CIDELO10__B,
       CACCU__CIDELO11__B CAC____CIDELO11__B,
       CACCU__CIDELO12__B CAC____CIDELO12__B,
       CACCU__CICRLO01__B CAC____CICRLO01__B,
       CACCU__CICRLO02__B CAC____CICRLO02__B,
       CACCU__CICRLO03__B CAC____CICRLO03__B,
       CACCU__CICRLO04__B CAC____CICRLO04__B,
       CACCU__CICRLO05__B CAC____CICRLO05__B,
       CACCU__CICRLO06__B CAC____CICRLO06__B,
       CACCU__CICRLO07__B CAC____CICRLO07__B,
       CACCU__CICRLO08__B CAC____CICRLO08__B,
       CACCU__CICRLO09__B CAC____CICRLO09__B,
       CACCU__CICRLO10__B CAC____CICRLO10__B,
       CACCU__CICRLO11__B CAC____CICRLO11__B,
       CACCU__CICRLO12__B CAC____CICRLO12__B,
       CACCU__CIDEOR01__B CAC____CIDEOR01__B,
       CACCU__CIDEOR02__B CAC____CIDEOR02__B,
       CACCU__CIDEOR03__B CAC____CIDEOR03__B,
       CACCU__CIDEOR04__B CAC____CIDEOR04__B,
       CACCU__CIDEOR05__B CAC____CIDEOR05__B,
       CACCU__CIDEOR06__B CAC____CIDEOR06__B,
       CACCU__CIDEOR07__B CAC____CIDEOR07__B,
       CACCU__CIDEOR08__B CAC____CIDEOR08__B,
       CACCU__CIDEOR09__B CAC____CIDEOR09__B,
       CACCU__CIDEOR10__B CAC____CIDEOR10__B,
       CACCU__CIDEOR11__B CAC____CIDEOR11__B,
       CACCU__CIDEOR12__B CAC____CIDEOR12__B,
       CACCU__CICROR01__B CAC____CICROR01__B,
       CACCU__CICROR02__B CAC____CICROR02__B,
       CACCU__CICROR03__B CAC____CICROR03__B,
       CACCU__CICROR04__B CAC____CICROR04__B,
       CACCU__CICROR05__B CAC____CICROR05__B,
       CACCU__CICROR06__B CAC____CICROR06__B,
       CACCU__CICROR07__B CAC____CICROR07__B,
       CACCU__CICROR08__B CAC____CICROR08__B,
       CACCU__CICROR09__B CAC____CICROR09__B,
       CACCU__CICROR10__B CAC____CICROR10__B,
       CACCU__CICROR11__B CAC____CICROR11__B,
       CACCU__CICROR12__B CAC____CICROR12__B,
       CACCU__AJUDEB01__B CAC____AJUDEB01__B,
       CACCU__AJUCRE01__B CAC____AJUCRE01__B,
       CACCU__AJUDEB02__B CAC____AJUDEB02__B,
       CACCU__AJUCRE02__B CAC____AJUCRE02__B,
       CACCU__AJUDEB03__B CAC____AJUDEB03__B,
       CACCU__AJUCRE03__B CAC____AJUCRE03__B,
       CACCU__AJUDEB04__B CAC____AJUDEB04__B,
       CACCU__AJUCRE04__B CAC____AJUCRE04__B,
       CACCU__AJUDEB05__B CAC____AJUDEB05__B,
       CACCU__AJUCRE05__B CAC____AJUCRE05__B,
       CACCU__AJUDEB06__B CAC____AJUDEB06__B,
       CACCU__AJUCRE06__B CAC____AJUCRE06__B,
       CACCU__AJUDEB07__B CAC____AJUDEB07__B,
       CACCU__AJUCRE07__B CAC____AJUCRE07__B,
       CACCU__AJUDEB08__B CAC____AJUDEB08__B,
       CACCU__AJUCRE08__B CAC____AJUCRE08__B,
       CACCU__AJUDEB09__B CAC____AJUDEB09__B,
       CACCU__AJUCRE09__B CAC____AJUCRE09__B,
       CACCU__AJUDEB10__B CAC____AJUDEB10__B,
       CACCU__AJUCRE10__B CAC____AJUCRE10__B,
       CACCU__AJUDEB11__B CAC____AJUDEB11__B,
       CACCU__AJUCRE11__B CAC____AJUCRE11__B,
       CACCU__AJUDEB12__B CAC____AJUDEB12__B,
       CACCU__AJUCRE12__B CAC____AJUCRE12__B
FROM SPCCU_, CACCU_, CPC___
WHERE SPCCU__CODIGO____CONTAB_B = CACCU__CODIGO____CONTAB_B
  AND SPCCU__CODIGO____PC_____B = CACCU__CODIGO____PC_____B
  AND SPCCU__CODIGO____CPC____B = CACCU__CODIGO____CPC____B
  AND SPCCU__CODIGO____PF_____B = CACCU__CODIGO____PF_____B
  AND SPCCU__CODIGO____CU_____B = CACCU__CODIGO____CU_____B
  AND SPCCU__CODIGO____PC_____B = CPC____CODIGO____PC_____B
  AND SPCCU__CODIGO____CPC____B = CPC____CODIGO____B
  --AND SPCCU__CODIGO____CONTAB_B = chrCodiCont__p
  AND SPCCU__CODIGO____PF_____B = chrCodPerFis_t
  AND SPCCU__CODIGO____CPC____B BETWEEN chrCodiCuen__p AND RPAD(chrCodiCuen__p, 26, 'Z')
  AND SPCCU__CODIGO____CU_____B BETWEEN chrCodCenIni_t AND chrCodCenFin_t
  AND (CPC____CODIGO____CONTAB_B IS NULL OR CPC____CODIGO____CONTAB_B = chrCodiCont__p)
ORDER BY SPCCU__CODIGO____CPC____B;

-- Cursor para recuperar los saldos por cuenta - tercero
CURSOR curSalCueTer_t IS
SELECT SPCT___CODIGO____CONTAB_B SPC____CODIGO____CONTAB_B,
       SPCT___CODIGO____PC_____B SPC____CODIGO____PC_____B,
       SPCT___CODIGO____CPC____B SPC____CODIGO____CPC____B,
       'xxxxxxxxxxxxxxxx' SPC____CODIGO____CU_____B,
       SPCT___IDENTIFIC_TERCER_B SPC____IDENTIFIC_TERCER_B,
       SPCT___CODIGO____PF_____B SPC____CODIGO____PF_____B,
       DECODE(CPC____NATUSALD__B, ConsDdls.chrDebito____c, 1, ConsDdls.chrCredito___c, -1)        CPC____NATUSALD__B,
       SPCT___SALINILOC_B SPC____SALINILOC_B,
       SPCT___ACDELO01__B SPC____ACDELO01__B,
       SPCT___ACCRLO01__B SPC____ACCRLO01__B,
       SPCT___ACDELO02__B SPC____ACDELO02__B,
       SPCT___ACCRLO02__B SPC____ACCRLO02__B,
       SPCT___ACDELO03__B SPC____ACDELO03__B,
       SPCT___ACCRLO03__B SPC____ACCRLO03__B,
       SPCT___ACDELO04__B SPC____ACDELO04__B,
       SPCT___ACCRLO04__B SPC____ACCRLO04__B,
       SPCT___ACDELO05__B SPC____ACDELO05__B,
       SPCT___ACCRLO05__B SPC____ACCRLO05__B,
       SPCT___ACDELO06__B SPC____ACDELO06__B,
       SPCT___ACCRLO06__B SPC____ACCRLO06__B,
       SPCT___ACDELO07__B SPC____ACDELO07__B,
       SPCT___ACCRLO07__B SPC____ACCRLO07__B,
       SPCT___ACDELO08__B SPC____ACDELO08__B,
       SPCT___ACCRLO08__B SPC____ACCRLO08__B,
       SPCT___ACDELO09__B SPC____ACDELO09__B,
       SPCT___ACCRLO09__B SPC____ACCRLO09__B,
       SPCT___ACDELO10__B SPC____ACDELO10__B,
       SPCT___ACCRLO10__B SPC____ACCRLO10__B,
       SPCT___ACDELO11__B SPC____ACDELO11__B,
       SPCT___ACCRLO11__B SPC____ACCRLO11__B,
       SPCT___ACDELO12__B SPC____ACDELO12__B,
       SPCT___ACCRLO12__B SPC____ACCRLO12__B,
       SPCT___SALINIORI_B SPC____SALINIORI_B,
       SPCT___ACDEOR01__B SPC____ACDEOR01__B,
       SPCT___ACCROR01__B SPC____ACCROR01__B,
       SPCT___ACDEOR02__B SPC____ACDEOR02__B,
       SPCT___ACCROR02__B SPC____ACCROR02__B,
       SPCT___ACDEOR03__B SPC____ACDEOR03__B,
       SPCT___ACCROR03__B SPC____ACCROR03__B,
       SPCT___ACDEOR04__B SPC____ACDEOR04__B,
       SPCT___ACCROR04__B SPC____ACCROR04__B,
       SPCT___ACDEOR05__B SPC____ACDEOR05__B,
       SPCT___ACCROR05__B SPC____ACCROR05__B,
       SPCT___ACDEOR06__B SPC____ACDEOR06__B,
       SPCT___ACCROR06__B SPC____ACCROR06__B,
       SPCT___ACDEOR07__B SPC____ACDEOR07__B,
       SPCT___ACCROR07__B SPC____ACCROR07__B,
       SPCT___ACDEOR08__B SPC____ACDEOR08__B,
       SPCT___ACCROR08__B SPC____ACCROR08__B,
       SPCT___ACDEOR09__B SPC____ACDEOR09__B,
       SPCT___ACCROR09__B SPC____ACCROR09__B,
       SPCT___ACDEOR10__B SPC____ACDEOR10__B,
       SPCT___ACCROR10__B SPC____ACCROR10__B,
       SPCT___ACDEOR11__B SPC____ACDEOR11__B,
       SPCT___ACCROR11__B SPC____ACCROR11__B,
       SPCT___ACDEOR12__B SPC____ACDEOR12__B,
       SPCT___ACCROR12__B SPC____ACCROR12__B,
       CACT___CIDELO01__B CAC____CIDELO01__B,
       CACT___CIDELO02__B CAC____CIDELO02__B,
       CACT___CIDELO03__B CAC____CIDELO03__B,
       CACT___CIDELO04__B CAC____CIDELO04__B,
       CACT___CIDELO05__B CAC____CIDELO05__B,
       CACT___CIDELO06__B CAC____CIDELO06__B,
       CACT___CIDELO07__B CAC____CIDELO07__B,
       CACT___CIDELO08__B CAC____CIDELO08__B,
       CACT___CIDELO09__B CAC____CIDELO09__B,
       CACT___CIDELO10__B CAC____CIDELO10__B,
       CACT___CIDELO11__B CAC____CIDELO11__B,
       CACT___CIDELO12__B CAC____CIDELO12__B,
       CACT___CICRLO01__B CAC____CICRLO01__B,
       CACT___CICRLO02__B CAC____CICRLO02__B,
       CACT___CICRLO03__B CAC____CICRLO03__B,
       CACT___CICRLO04__B CAC____CICRLO04__B,
       CACT___CICRLO05__B CAC____CICRLO05__B,
       CACT___CICRLO06__B CAC____CICRLO06__B,
       CACT___CICRLO07__B CAC____CICRLO07__B,
       CACT___CICRLO08__B CAC____CICRLO08__B,
       CACT___CICRLO09__B CAC____CICRLO09__B,
       CACT___CICRLO10__B CAC____CICRLO10__B,
       CACT___CICRLO11__B CAC____CICRLO11__B,
       CACT___CICRLO12__B CAC____CICRLO12__B,
       CACT___CIDEOR01__B CAC____CIDEOR01__B,
       CACT___CIDEOR02__B CAC____CIDEOR02__B,
       CACT___CIDEOR03__B CAC____CIDEOR03__B,
       CACT___CIDEOR04__B CAC____CIDEOR04__B,
       CACT___CIDEOR05__B CAC____CIDEOR05__B,
       CACT___CIDEOR06__B CAC____CIDEOR06__B,
       CACT___CIDEOR07__B CAC____CIDEOR07__B,
       CACT___CIDEOR08__B CAC____CIDEOR08__B,
       CACT___CIDEOR09__B CAC____CIDEOR09__B,
       CACT___CIDEOR10__B CAC____CIDEOR10__B,
       CACT___CIDEOR11__B CAC____CIDEOR11__B,
       CACT___CIDEOR12__B CAC____CIDEOR12__B,
       CACT___CICROR01__B CAC____CICROR01__B,
       CACT___CICROR02__B CAC____CICROR02__B,
       CACT___CICROR03__B CAC____CICROR03__B,
       CACT___CICROR04__B CAC____CICROR04__B,
       CACT___CICROR05__B CAC____CICROR05__B,
       CACT___CICROR06__B CAC____CICROR06__B,
       CACT___CICROR07__B CAC____CICROR07__B,
       CACT___CICROR08__B CAC____CICROR08__B,
       CACT___CICROR09__B CAC____CICROR09__B,
       CACT___CICROR10__B CAC____CICROR10__B,
       CACT___CICROR11__B CAC____CICROR11__B,
       CACT___CICROR12__B CAC____CICROR12__B,
       CACT___AJUDEB01__B CAC____AJUDEB01__B,
       CACT___AJUCRE01__B CAC____AJUCRE01__B,
       CACT___AJUDEB02__B CAC____AJUDEB02__B,
       CACT___AJUCRE02__B CAC____AJUCRE02__B,
       CACT___AJUDEB03__B CAC____AJUDEB03__B,
       CACT___AJUCRE03__B CAC____AJUCRE03__B,
       CACT___AJUDEB04__B CAC____AJUDEB04__B,
       CACT___AJUCRE04__B CAC____AJUCRE04__B,
       CACT___AJUDEB05__B CAC____AJUDEB05__B,
       CACT___AJUCRE05__B CAC____AJUCRE05__B,
       CACT___AJUDEB06__B CAC____AJUDEB06__B,
       CACT___AJUCRE06__B CAC____AJUCRE06__B,
       CACT___AJUDEB07__B CAC____AJUDEB07__B,
       CACT___AJUCRE07__B CAC____AJUCRE07__B,
       CACT___AJUDEB08__B CAC____AJUDEB08__B,
       CACT___AJUCRE08__B CAC____AJUCRE08__B,
       CACT___AJUDEB09__B CAC____AJUDEB09__B,
       CACT___AJUCRE09__B CAC____AJUCRE09__B,
       CACT___AJUDEB10__B CAC____AJUDEB10__B,
       CACT___AJUCRE10__B CAC____AJUCRE10__B,
       CACT___AJUDEB11__B CAC____AJUDEB11__B,
       CACT___AJUCRE11__B CAC____AJUCRE11__B,
       CACT___AJUDEB12__B CAC____AJUDEB12__B,
       CACT___AJUCRE12__B CAC____AJUCRE12__B
FROM SPCT__, CACT__, CPC___
WHERE SPCT___CODIGO____CONTAB_B = CACT___CODIGO____CONTAB_B
  AND SPCT___CODIGO____PC_____B = CACT___CODIGO____PC_____B
  AND SPCT___CODIGO____CPC____B = CACT___CODIGO____CPC____B
  AND SPCT___CODIGO____PF_____B = CACT___CODIGO____PF_____B
  AND SPCT___IDENTIFIC_TERCER_B = CACT___IDENTIFIC_TERCER_B
  AND SPCT___CODIGO____PC_____B = CPC____CODIGO____PC_____B
  AND SPCT___CODIGO____CPC____B = CPC____CODIGO____B
  --AND SPCT___CODIGO____CONTAB_B = chrCodiCont__p
  AND SPCT___CODIGO____PF_____B = chrCodPerFis_t
  AND SPCT___CODIGO____CPC____B BETWEEN chrCodiCuen__p AND RPAD(chrCodiCuen__p, 26, 'Z')
  AND SPCT___IDENTIFIC_TERCER_B = intIdenTerc__p
  AND (CPC____CODIGO____CONTAB_B IS NULL OR CPC____CODIGO____CONTAB_B = chrCodiCont__p) 
ORDER BY SPCT___CODIGO____CPC____B;

-- Cursor para recuperar los saldos por cuenta - centro - tercer
CURSOR curSaCuCeTe__t IS
SELECT SPCCUT_CODIGO____CONTAB_B SPC____CODIGO____CONTAB_B,
       SPCCUT_CODIGO____PC_____B SPC____CODIGO____PC_____B,
       SPCCUT_CODIGO____CPC____B SPC____CODIGO____CPC____B,
       SPCCUT_CODIGO____CU_____B SPC____CODIGO____CU_____B,
       SPCCUT_IDENTIFIC_TERCER_B SPC____IDENTIFIC_TERCER_B,
       SPCCUT_CODIGO____PF_____B SPC____CODIGO____PF_____B,
       DECODE(CPC____NATUSALD__B, ConsDdls.chrDebito____c, 1, ConsDdls.chrCredito___c, -1)        CPC____NATUSALD__B,
       SPCCUT_SALINILOC_B SPC____SALINILOC_B,
       SPCCUT_ACDELO01__B SPC____ACDELO01__B,
       SPCCUT_ACCRLO01__B SPC____ACCRLO01__B,
       SPCCUT_ACDELO02__B SPC____ACDELO02__B,
       SPCCUT_ACCRLO02__B SPC____ACCRLO02__B,
       SPCCUT_ACDELO03__B SPC____ACDELO03__B,
       SPCCUT_ACCRLO03__B SPC____ACCRLO03__B,
       SPCCUT_ACDELO04__B SPC____ACDELO04__B,
       SPCCUT_ACCRLO04__B SPC____ACCRLO04__B,
       SPCCUT_ACDELO05__B SPC____ACDELO05__B,
       SPCCUT_ACCRLO05__B SPC____ACCRLO05__B,
       SPCCUT_ACDELO06__B SPC____ACDELO06__B,
       SPCCUT_ACCRLO06__B SPC____ACCRLO06__B,
       SPCCUT_ACDELO07__B SPC____ACDELO07__B,
       SPCCUT_ACCRLO07__B SPC____ACCRLO07__B,
       SPCCUT_ACDELO08__B SPC____ACDELO08__B,
       SPCCUT_ACCRLO08__B SPC____ACCRLO08__B,
       SPCCUT_ACDELO09__B SPC____ACDELO09__B,
       SPCCUT_ACCRLO09__B SPC____ACCRLO09__B,
       SPCCUT_ACDELO10__B SPC____ACDELO10__B,
       SPCCUT_ACCRLO10__B SPC____ACCRLO10__B,
       SPCCUT_ACDELO11__B SPC____ACDELO11__B,
       SPCCUT_ACCRLO11__B SPC____ACCRLO11__B,
       SPCCUT_ACDELO12__B SPC____ACDELO12__B,
       SPCCUT_ACCRLO12__B SPC____ACCRLO12__B,
       SPCCUT_SALINIORI_B SPC____SALINIORI_B,
       SPCCUT_ACDEOR01__B SPC____ACDEOR01__B,
       SPCCUT_ACCROR01__B SPC____ACCROR01__B,
       SPCCUT_ACDEOR02__B SPC____ACDEOR02__B,
       SPCCUT_ACCROR02__B SPC____ACCROR02__B,
       SPCCUT_ACDEOR03__B SPC____ACDEOR03__B,
       SPCCUT_ACCROR03__B SPC____ACCROR03__B,
       SPCCUT_ACDEOR04__B SPC____ACDEOR04__B,
       SPCCUT_ACCROR04__B SPC____ACCROR04__B,
       SPCCUT_ACDEOR05__B SPC____ACDEOR05__B,
       SPCCUT_ACCROR05__B SPC____ACCROR05__B,
       SPCCUT_ACDEOR06__B SPC____ACDEOR06__B,
       SPCCUT_ACCROR06__B SPC____ACCROR06__B,
       SPCCUT_ACDEOR07__B SPC____ACDEOR07__B,
       SPCCUT_ACCROR07__B SPC____ACCROR07__B,
       SPCCUT_ACDEOR08__B SPC____ACDEOR08__B,
       SPCCUT_ACCROR08__B SPC____ACCROR08__B,
       SPCCUT_ACDEOR09__B SPC____ACDEOR09__B,
       SPCCUT_ACCROR09__B SPC____ACCROR09__B,
       SPCCUT_ACDEOR10__B SPC____ACDEOR10__B,
       SPCCUT_ACCROR10__B SPC____ACCROR10__B,
       SPCCUT_ACDEOR11__B SPC____ACDEOR11__B,
       SPCCUT_ACCROR11__B SPC____ACCROR11__B,
       SPCCUT_ACDEOR12__B SPC____ACDEOR12__B,
       SPCCUT_ACCROR12__B SPC____ACCROR12__B,
       CACCUT_CIDELO01__B CAC____CIDELO01__B,
       CACCUT_CIDELO02__B CAC____CIDELO02__B,
       CACCUT_CIDELO03__B CAC____CIDELO03__B,
       CACCUT_CIDELO04__B CAC____CIDELO04__B,
       CACCUT_CIDELO05__B CAC____CIDELO05__B,
       CACCUT_CIDELO06__B CAC____CIDELO06__B,
       CACCUT_CIDELO07__B CAC____CIDELO07__B,
       CACCUT_CIDELO08__B CAC____CIDELO08__B,
       CACCUT_CIDELO09__B CAC____CIDELO09__B,
       CACCUT_CIDELO10__B CAC____CIDELO10__B,
       CACCUT_CIDELO11__B CAC____CIDELO11__B,
       CACCUT_CIDELO12__B CAC____CIDELO12__B,
       CACCUT_CICRLO01__B CAC____CICRLO01__B,
       CACCUT_CICRLO02__B CAC____CICRLO02__B,
       CACCUT_CICRLO03__B CAC____CICRLO03__B,
       CACCUT_CICRLO04__B CAC____CICRLO04__B,
       CACCUT_CICRLO05__B CAC____CICRLO05__B,
       CACCUT_CICRLO06__B CAC____CICRLO06__B,
       CACCUT_CICRLO07__B CAC____CICRLO07__B,
       CACCUT_CICRLO08__B CAC____CICRLO08__B,
       CACCUT_CICRLO09__B CAC____CICRLO09__B,
       CACCUT_CICRLO10__B CAC____CICRLO10__B,
       CACCUT_CICRLO11__B CAC____CICRLO11__B,
       CACCUT_CICRLO12__B CAC____CICRLO12__B,
       CACCUT_CIDEOR01__B CAC____CIDEOR01__B,
       CACCUT_CIDEOR02__B CAC____CIDEOR02__B,
       CACCUT_CIDEOR03__B CAC____CIDEOR03__B,
       CACCUT_CIDEOR04__B CAC____CIDEOR04__B,
       CACCUT_CIDEOR05__B CAC____CIDEOR05__B,
       CACCUT_CIDEOR06__B CAC____CIDEOR06__B,
       CACCUT_CIDEOR07__B CAC____CIDEOR07__B,
       CACCUT_CIDEOR08__B CAC____CIDEOR08__B,
       CACCUT_CIDEOR09__B CAC____CIDEOR09__B,
       CACCUT_CIDEOR10__B CAC____CIDEOR10__B,
       CACCUT_CIDEOR11__B CAC____CIDEOR11__B,
       CACCUT_CIDEOR12__B CAC____CIDEOR12__B,
       CACCUT_CICROR01__B CAC____CICROR01__B,
       CACCUT_CICROR02__B CAC____CICROR02__B,
       CACCUT_CICROR03__B CAC____CICROR03__B,
       CACCUT_CICROR04__B CAC____CICROR04__B,
       CACCUT_CICROR05__B CAC____CICROR05__B,
       CACCUT_CICROR06__B CAC____CICROR06__B,
       CACCUT_CICROR07__B CAC____CICROR07__B,
       CACCUT_CICROR08__B CAC____CICROR08__B,
       CACCUT_CICROR09__B CAC____CICROR09__B,
       CACCUT_CICROR10__B CAC____CICROR10__B,
       CACCUT_CICROR11__B CAC____CICROR11__B,
       CACCUT_CICROR12__B CAC____CICROR12__B,
       CACCUT_AJUDEB01__B CAC____AJUDEB01__B,
       CACCUT_AJUCRE01__B CAC____AJUCRE01__B,
       CACCUT_AJUDEB02__B CAC____AJUDEB02__B,
       CACCUT_AJUCRE02__B CAC____AJUCRE02__B,
       CACCUT_AJUDEB03__B CAC____AJUDEB03__B,
       CACCUT_AJUCRE03__B CAC____AJUCRE03__B,
       CACCUT_AJUDEB04__B CAC____AJUDEB04__B,
       CACCUT_AJUCRE04__B CAC____AJUCRE04__B,
       CACCUT_AJUDEB05__B CAC____AJUDEB05__B,
       CACCUT_AJUCRE05__B CAC____AJUCRE05__B,
       CACCUT_AJUDEB06__B CAC____AJUDEB06__B,
       CACCUT_AJUCRE06__B CAC____AJUCRE06__B,
       CACCUT_AJUDEB07__B CAC____AJUDEB07__B,
       CACCUT_AJUCRE07__B CAC____AJUCRE07__B,
       CACCUT_AJUDEB08__B CAC____AJUDEB08__B,
       CACCUT_AJUCRE08__B CAC____AJUCRE08__B,
       CACCUT_AJUDEB09__B CAC____AJUDEB09__B,
       CACCUT_AJUCRE09__B CAC____AJUCRE09__B,
       CACCUT_AJUDEB10__B CAC____AJUDEB10__B,
       CACCUT_AJUCRE10__B CAC____AJUCRE10__B,
       CACCUT_AJUDEB11__B CAC____AJUDEB11__B,
       CACCUT_AJUCRE11__B CAC____AJUCRE11__B,
       CACCUT_AJUDEB12__B CAC____AJUDEB12__B,
       CACCUT_AJUCRE12__B CAC____AJUCRE12__B
FROM SPCCUT, CACCUT, CPC___
WHERE SPCCUT_CODIGO____CONTAB_B = CACCUT_CODIGO____CONTAB_B
  AND SPCCUT_CODIGO____PC_____B = CACCUT_CODIGO____PC_____B
  AND SPCCUT_CODIGO____CPC____B = CACCUT_CODIGO____CPC____B
  AND SPCCUT_CODIGO____PF_____B = CACCUT_CODIGO____PF_____B
  AND SPCCUT_CODIGO____CU_____B = CACCUT_CODIGO____CU_____B
  AND SPCCUT_IDENTIFIC_TERCER_B = CACCUT_IDENTIFIC_TERCER_B
  AND SPCCUT_CODIGO____PC_____B = CPC____CODIGO____PC_____B
  AND SPCCUT_CODIGO____CPC____B = CPC____CODIGO____B
  --AND SPCCUT_CODIGO____CONTAB_B = chrCodiCont__p
  AND SPCCUT_CODIGO____PF_____B = chrCodPerFis_t
  AND SPCCUT_CODIGO____CPC____B BETWEEN chrCodiCuen__p AND RPAD(chrCodiCuen__p, 26, 'Z')
  AND SPCCUT_CODIGO____CU_____B BETWEEN chrCodCenIni_t AND chrCodCenFin_t
  AND SPCCUT_IDENTIFIC_TERCER_B = intIdenTerc__p
  --AND (CPC____CODIGO____CONTAB_B IS NULL OR CPC____CODIGO____CONTAB_B = chrCodiCont__p)
ORDER BY SPCCUT_CODIGO____CPC____B;

-- Cursor para recuperar el movimiento de una cuenta
CURSOR curMoviCuen__t IS
SELECT MC_____CODIGO____CPC____B,
       SUM(MC_____DEBMONORI_B) MC_____DEBMONORI_B,
       SUM(MC_____CREMONORI_B) MC_____CREMONORI_B,
       SUM(MC_____DEBMONLOC_B) MC_____DEBMONLOC_B,
       SUM(MC_____CREMONLOC_B) MC_____CREMONLOC_B
FROM MC____
WHERE --MC_____CODIGO____CONTAB_B = chrCodiCont__p AND
      MC_____CODIGO____PF_____B = chrCodPerFis_t
  AND MC_____CODIGO____CPC____B BETWEEN chrCodiCuen__p AND RPAD(chrCodiCuen__p,26,'Z')
  AND MC_____FECHA_____B BETWEEN chrFecIniPer_t AND chrFechSald__p
  AND ((MC_____INDTIPMOV_B <> ConsDdls.intMoviCier__c AND MC_____INDTIPMOV_B <> ConsDdls.intMoviAjus__c) OR
       (MC_____INDTIPMOV_B = ConsDdls.intMoviCier__c AND chrInclCier__p = 'S') OR
       (MC_____INDTIPMOV_B = ConsDdls.intMoviAjus__c AND chrInclAjus__p = 'S'))
GROUP BY MC_____CODIGO____CPC____B;

-- Cursor para recuperar el movimiento de una cuenta - centro
CURSOR curMovCueCen_t IS
SELECT MC_____CODIGO____CPC____B,
       SUM(MC_____DEBMONORI_B) MC_____DEBMONORI_B,
       SUM(MC_____CREMONORI_B) MC_____CREMONORI_B,
       SUM(MC_____DEBMONLOC_B) MC_____DEBMONLOC_B,
       SUM(MC_____CREMONLOC_B) MC_____CREMONLOC_B
FROM MC____
WHERE --MC_____CODIGO____CONTAB_B = chrCodiCont__p AND
      MC_____CODIGO____PF_____B = chrCodPerFis_t
  AND MC_____CODIGO____CPC____B BETWEEN chrCodiCuen__p AND RPAD(chrCodiCuen__p,26,'Z')
  AND MC_____CODIGO____CU_____B BETWEEN chrCodCenIni_t AND chrCodCenFin_t
  AND MC_____FECHA_____B BETWEEN chrFecIniPer_t AND chrFechSald__p
  AND ((MC_____INDTIPMOV_B <> ConsDdls.intMoviCier__c AND MC_____INDTIPMOV_B <> ConsDdls.intMoviAjus__c) OR
       (MC_____INDTIPMOV_B = ConsDdls.intMoviCier__c AND chrInclCier__p = 'S') OR
       (MC_____INDTIPMOV_B = ConsDdls.intMoviAjus__c AND chrInclAjus__p = 'S'))
GROUP BY MC_____CODIGO____CPC____B;

-- Cursor para recuperar el movimiento de una cuenta - tercero
CURSOR curMovCueTer_t IS
SELECT MC_____CODIGO____CPC____B,
       SUM(MC_____DEBMONORI_B) MC_____DEBMONORI_B,
       SUM(MC_____CREMONORI_B) MC_____CREMONORI_B,
       SUM(MC_____DEBMONLOC_B) MC_____DEBMONLOC_B,
       SUM(MC_____CREMONLOC_B) MC_____CREMONLOC_B
FROM MC____
WHERE --MC_____CODIGO____CONTAB_B = chrCodiCont__p AND
      MC_____CODIGO____PF_____B = chrCodPerFis_t
  AND MC_____CODIGO____CPC____B BETWEEN chrCodiCuen__p AND RPAD(chrCodiCuen__p,26,'Z')
  AND MC_____IDENTIFIC_TERCER_B = intIdenTerc__p
  AND MC_____FECHA_____B BETWEEN chrFecIniPer_t AND chrFechSald__p
  AND ((MC_____INDTIPMOV_B <> ConsDdls.intMoviCier__c AND MC_____INDTIPMOV_B <> ConsDdls.intMoviAjus__c) OR
       (MC_____INDTIPMOV_B = ConsDdls.intMoviCier__c AND chrInclCier__p = 'S') OR
       (MC_____INDTIPMOV_B = ConsDdls.intMoviAjus__c AND chrInclAjus__p = 'S'))
GROUP BY MC_____CODIGO____CPC____B;

-- Cursor para recuperar el movimiento de una cuenta - centro - tercero
CURSOR curMoCuCeTe__t IS
SELECT MC_____CODIGO____CPC____B,
       SUM(MC_____DEBMONORI_B) MC_____DEBMONORI_B,
       SUM(MC_____CREMONORI_B) MC_____CREMONORI_B,
       SUM(MC_____DEBMONLOC_B) MC_____DEBMONLOC_B,
       SUM(MC_____CREMONLOC_B) MC_____CREMONLOC_B
FROM MC____
WHERE --MC_____CODIGO____CONTAB_B = chrCodiCont__p AND
      MC_____CODIGO____PF_____B = chrCodPerFis_t
  AND MC_____CODIGO____CPC____B BETWEEN chrCodiCuen__p AND RPAD(chrCodiCuen__p,26,'Z')
  AND MC_____CODIGO____CU_____B BETWEEN chrCodCenIni_t AND chrCodCenFin_t
  AND MC_____IDENTIFIC_TERCER_B = intIdenTerc__p
  AND MC_____FECHA_____B BETWEEN chrFecIniPer_t AND chrFechSald__p
  AND ((MC_____INDTIPMOV_B <> ConsDdls.intMoviCier__c AND MC_____INDTIPMOV_B <> ConsDdls.intMoviAjus__c) OR
       (MC_____INDTIPMOV_B = ConsDdls.intMoviCier__c AND chrInclCier__p = 'S') OR
       (MC_____INDTIPMOV_B = ConsDdls.intMoviAjus__c AND chrInclAjus__p = 'S'))
GROUP BY MC_____CODIGO____CPC____B;

regSaldCuen__t curSaldCuen__t%ROWTYPE;
regMoviCuen__t curMoviCuen__t%ROWTYPE;

  PROCEDURE CalcularSaldos(regSaldos____p IN curSaldCuen__t%ROWTYPE) IS
  BEGIN
    -- Acumula el saldo inicial de la cuenta
    IF regSaldos____p.CPC____NATUSALD__B = intNatuCuen__t THEN
      dblSalIniLoc_g := dblSalIniLoc_g + regSaldos____p.SPC____SALINILOC_B;
      dblSalIniOri_g := dblSalIniOri_g + regSaldos____p.SPC____SALINIORI_B;
    ELSE
      dblSalIniLoc_g := dblSalIniLoc_g - regSaldos____p.SPC____SALINILOC_B;
      dblSalIniOri_g := dblSalIniOri_g - regSaldos____p.SPC____SALINIORI_B;
    END IF;

    IF intUltPerSal_t >= 1 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO01__B, regSaldos____p.SPC____ACCRLO01__B,
                           regSaldos____p.SPC____ACDEOR01__B, regSaldos____p.SPC____ACCROR01__B,
                           regSaldos____p.CAC____AJUDEB01__B, regSaldos____p.CAC____AJUCRE01__B,
                           regSaldos____p.CAC____CIDELO01__B, regSaldos____p.CAC____CICRLO01__B,
                           regSaldos____p.CAC____CIDEOR01__B, regSaldos____p.CAC____CICROR01__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 2 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO02__B, regSaldos____p.SPC____ACCRLO02__B,
                           regSaldos____p.SPC____ACDEOR02__B, regSaldos____p.SPC____ACCROR02__B,
                           regSaldos____p.CAC____AJUDEB02__B, regSaldos____p.CAC____AJUCRE02__B,
                           regSaldos____p.CAC____CIDELO02__B, regSaldos____p.CAC____CICRLO02__B,
                           regSaldos____p.CAC____CIDEOR02__B, regSaldos____p.CAC____CICROR02__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 3 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO03__B, regSaldos____p.SPC____ACCRLO03__B,
                           regSaldos____p.SPC____ACDEOR03__B, regSaldos____p.SPC____ACCROR03__B,
                           regSaldos____p.CAC____AJUDEB03__B, regSaldos____p.CAC____AJUCRE03__B,
                           regSaldos____p.CAC____CIDELO03__B, regSaldos____p.CAC____CICRLO03__B,
                           regSaldos____p.CAC____CIDEOR03__B, regSaldos____p.CAC____CICROR03__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 4 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO04__B, regSaldos____p.SPC____ACCRLO04__B,
                           regSaldos____p.SPC____ACDEOR04__B, regSaldos____p.SPC____ACCROR04__B,
                           regSaldos____p.CAC____AJUDEB04__B, regSaldos____p.CAC____AJUCRE04__B,
                           regSaldos____p.CAC____CIDELO04__B, regSaldos____p.CAC____CICRLO04__B,
                           regSaldos____p.CAC____CIDEOR04__B, regSaldos____p.CAC____CICROR04__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 5 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO05__B, regSaldos____p.SPC____ACCRLO05__B,
                           regSaldos____p.SPC____ACDEOR05__B, regSaldos____p.SPC____ACCROR05__B,
                           regSaldos____p.CAC____AJUDEB05__B, regSaldos____p.CAC____AJUCRE05__B,
                           regSaldos____p.CAC____CIDELO05__B, regSaldos____p.CAC____CICRLO05__B,
                           regSaldos____p.CAC____CIDEOR05__B, regSaldos____p.CAC____CICROR05__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 6 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO06__B, regSaldos____p.SPC____ACCRLO06__B,
                           regSaldos____p.SPC____ACDEOR06__B, regSaldos____p.SPC____ACCROR06__B,
                           regSaldos____p.CAC____AJUDEB06__B, regSaldos____p.CAC____AJUCRE06__B,
                           regSaldos____p.CAC____CIDELO06__B, regSaldos____p.CAC____CICRLO06__B,
                           regSaldos____p.CAC____CIDEOR06__B, regSaldos____p.CAC____CICROR06__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 7 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO07__B, regSaldos____p.SPC____ACCRLO07__B,
                           regSaldos____p.SPC____ACDEOR07__B, regSaldos____p.SPC____ACCROR07__B,
                           regSaldos____p.CAC____AJUDEB07__B, regSaldos____p.CAC____AJUCRE07__B,
                           regSaldos____p.CAC____CIDELO07__B, regSaldos____p.CAC____CICRLO07__B,
                           regSaldos____p.CAC____CIDEOR07__B, regSaldos____p.CAC____CICROR07__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 8 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO08__B, regSaldos____p.SPC____ACCRLO08__B,
                           regSaldos____p.SPC____ACDEOR08__B, regSaldos____p.SPC____ACCROR08__B,
                           regSaldos____p.CAC____AJUDEB08__B, regSaldos____p.CAC____AJUCRE08__B,
                           regSaldos____p.CAC____CIDELO08__B, regSaldos____p.CAC____CICRLO08__B,
                           regSaldos____p.CAC____CIDEOR08__B, regSaldos____p.CAC____CICROR08__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 9 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO09__B, regSaldos____p.SPC____ACCRLO09__B,
                           regSaldos____p.SPC____ACDEOR09__B, regSaldos____p.SPC____ACCROR09__B,
                           regSaldos____p.CAC____AJUDEB09__B, regSaldos____p.CAC____AJUCRE09__B,
                           regSaldos____p.CAC____CIDELO09__B, regSaldos____p.CAC____CICRLO09__B,
                           regSaldos____p.CAC____CIDEOR09__B, regSaldos____p.CAC____CICROR09__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 10 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO10__B, regSaldos____p.SPC____ACCRLO10__B,
                           regSaldos____p.SPC____ACDEOR10__B, regSaldos____p.SPC____ACCROR10__B,
                           regSaldos____p.CAC____AJUDEB10__B, regSaldos____p.CAC____AJUCRE10__B,
                           regSaldos____p.CAC____CIDELO10__B, regSaldos____p.CAC____CICRLO10__B,
                           regSaldos____p.CAC____CIDEOR10__B, regSaldos____p.CAC____CICROR10__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 11 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO11__B, regSaldos____p.SPC____ACCRLO11__B,
                           regSaldos____p.SPC____ACDEOR11__B, regSaldos____p.SPC____ACCROR11__B,
                           regSaldos____p.CAC____AJUDEB11__B, regSaldos____p.CAC____AJUCRE11__B,
                           regSaldos____p.CAC____CIDELO11__B, regSaldos____p.CAC____CICRLO11__B,
                           regSaldos____p.CAC____CIDEOR11__B, regSaldos____p.CAC____CICROR11__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
    IF intUltPerSal_t >= 12 THEN
      AcumularSaldosCuenta(regSaldos____p.SPC____ACDELO12__B, regSaldos____p.SPC____ACCRLO12__B,
                           regSaldos____p.SPC____ACDEOR12__B, regSaldos____p.SPC____ACCROR12__B,
                           regSaldos____p.CAC____AJUDEB12__B, regSaldos____p.CAC____AJUCRE12__B,
                           regSaldos____p.CAC____CIDELO12__B, regSaldos____p.CAC____CICRLO12__B,
                           regSaldos____p.CAC____CIDEOR12__B, regSaldos____p.CAC____CICROR12__B,
                           chrInclCier__p,chrInclAjus__p);
    END IF;
  END CalcularSaldos;

BEGIN
  --
  -- INICIALIZA LAS VARIABLES
  --
  -- Valores que se van a retornar  
  
  dblSalIniLoc_g := 0;
  dblSalIniOri_g := 0;
  dblDebiLoca__g := 0;
  dblCredLoca__g := 0;
  dblDebiOrig__g := 0;
  dblCredOrig__g := 0;
  dblSalFinLoc_g := 0;
  dblSalFinOri_g := 0;
  chrIndHijSal_g := 'N';

  -- Planes de la contabilidad  
  --ALBV01 REQ. 41R1-004984. SE VALIDA PARA CUANDO LA VARIABLE GLOGAL SEA NULA. PARA QUE GENERE SALDOS
  --EN FOGAFIN SI SE DEJA CON LA VALIDACION IS NOT NULL NO GENERA SALDOS.
  --IF chrCodPlaCue_g IS NOT NULL THEN
  IF chrCodPlaCue_g IS NULL THEN
    BEGIN
      SELECT DECODE(PC_____INDPLAESP_B, 'N', PC_____CODIGO____B, PC_____CODIGO____PC_____B),  -- Plan básico
             DECODE(PC_____INDPLAESP_B, 'S', PC_____CODIGO____B, '-1')                        -- Plan específico
      INTO chrCodPlaBas_t, chrCodPlaEsp_t
      FROM CONTAB, PC____
      WHERE CONTAB_CODIGO____PC_____B = PC_____CODIGO____B
        AND CONTAB_CODIGO____B = chrCodiCont__p;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20050, 'No existe la contabilidad: ' || chrCodiCont__p || '|');
      WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR(-20050, SQLERRM);
    END;
  ELSE
    -- Req. 4824
    chrCodPlaBas_t := chrCodPlaCue_g;
    chrCodPlaEsp_t := 'xxxx';
  END IF;

  -- Se determina el período de la fecha actual
  --ALBV01 REQ. 41R1-004984. SE VALIDA PARA CUANDO LA VARIABLE GLOGAL SEA NULA. PARA QUE GENERE SALDOS
  --EN FOGAFIN SI SE DEJA CON LA VALIDACION IS NOT NULL NO GENERA SALDOS.
  --IF chrCodPerFis_g IS NOT NULL THEN
  IF chrCodPerFis_g IS NULL THEN
    BEGIN
      SELECT PERIOD_CODIGO____PF_____B, PERIOD_NUMERO____B, 
             PERIOD_FECHINIC__B, PERIOD_FECHFINA__B
      INTO chrCodPerFis_t, intNumePeri__t, chrFecIniPer_t, chrFecFinPer_t
      FROM PERIOD
      WHERE chrFechSald__p BETWEEN PERIOD_FECHINIC__B AND PERIOD_FECHFINA__B;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20050, 'La fecha no corresponde a ningún período (' || chrFechSald__p || ')|');
    END;
  ELSE
    -- Req. 4824
    chrCodPerFis_t := chrCodPerFis_g;
    intNumePeri__t := intNumePeri__g;
    chrFecIniPer_t := chrFecIniPer_g;
    chrFecFinPer_t := chrFecFinPer_g;
  END IF;

  IF chrFecFinPer_t <> chrFechSald__p THEN
    -- No se pide el saldo al final de un período, se debe buscar en MC____ y el
    -- último período que se toma de saldos es el anterior al de la fecha especificada.
    chrIndBusMC__t := 'S';
    intUltPerSal_t := intNumePeri__t - 1;
  ELSE
    -- Todos los valores se toman de saldos y hasta el período de la fecha especificada.
    chrIndBusMC__t := 'N';
    intUltPerSal_t := intNumePeri__t;
  END IF;

  -- Determina la naturaleza de la cuenta
  --ALBV01 REQ. 41R1-004984. SE VALIDA PARA CUANDO LA VARIABLE GLOGAL SEA NULA. PARA QUE GENERE SALDOS.
  --EN FOGAFIN SI SE DEJA CON LA VALIDACION IS NOT NULL NO GENERA SALDOS.
  --IF chrNatuSald__g IS NOT NULL THEN
  IF chrNatuSald__g IS NULL THEN
    BEGIN
      SELECT DECODE(CPC____NATUSALD__B, Consddls.chrDebito____c, 1, Consddls.chrCredito___c, -1)
      INTO intNatuCuen__t
      FROM CPC___
      WHERE CPC____CODIGO____PC_____B IN (chrCodPlaBas_t, chrCodPlaEsp_t)
        AND CPC____CODIGO____B = chrCodiCuen__p;
      intFacNatCue_g := intNatuCuen__t;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20050, 'No existe la cuenta ' || chrCodiCuen__p || '|');
    END;
  ELSE
    -- Req. 4824
    IF chrNatuSald__g = Consddls.chrDebito____c THEN
      intFacNatCue_g := 1;
      --ALBV01 Req. 41R1-005006. Asigna el valor correspondiente y evita dejar la variable con valor nulo
      intNatuCuen__t := 1;
    ELSE
      intFacNatCue_g := -1;
      --ALBV01 Req. 41R1-005006. Asigna el valor correspondiente y evita dejar la variable con valor nulo
      intNatuCuen__t := -1;
    END IF;
  END IF;

  -- Define los centros inicial y final a consultar
  chrCodCenIni_t := chrCodCenUti_p;
  IF chrIncArbCen_p = 'S' THEN
    chrCodCenFin_t := SUBSTR(chrCodCenIni_t || 'ZZZZZZZZZZZZZZZZ', 1, 16);
  ELSE
    chrCodCenFin_t := chrCodCenIni_t;
  END IF;

  -- Recorre el cursor correspondiente para calcular los saldos
  IF chrCodCenUti_p IS NULL AND intIdenTerc__p IS NULL THEN
    -- Saldos por cuenta
    OPEN curSaldCuen__t;
    FETCH curSaldCuen__t INTO regSaldCuen__t;
    WHILE curSaldCuen__t%FOUND LOOP
      CALCULARSALDOS(regSaldCuen__t);
      FETCH curSaldCuen__t INTO regSaldCuen__t;
    END LOOP;

    -- Movimiento por cuenta
    IF chrIndBusMC__t = 'S' THEN
      OPEN curMoviCuen__t;
      FETCH curMoviCuen__t INTO regMoviCuen__t;
      WHILE curMoviCuen__t%FOUND LOOP
        dblDebiLoca__g := dblDebiLoca__g + NVL(regMoviCuen__t.MC_____DEBMONLOC_B, 0);
        dblCredLoca__g := dblCredLoca__g + NVL(regMoviCuen__t.MC_____CREMONLOC_B, 0);
        dblDebiOrig__g := dblDebiOrig__g + NVL(regMoviCuen__t.MC_____DEBMONORI_B, 0);
        dblCredOrig__g := dblCredOrig__g + NVL(regMoviCuen__t.MC_____CREMONORI_B, 0);        
        FETCH curMoviCuen__t INTO regMoviCuen__t;
      END LOOP;
    END IF;
  END IF;

  IF chrCodCenUti_p IS NOT NULL AND intIdenTerc__p IS NULL THEN
    -- Saldos por cuenta - centro    
    OPEN curSalCueCen_t;        
    FETCH curSalCueCen_t INTO regSaldCuen__t;        
    WHILE curSalCueCen_t%FOUND LOOP      
      CALCULARSALDOS(regSaldCuen__t);
      FETCH curSalCueCen_t INTO regSaldCuen__t;
    END LOOP;    
    -- Movimiento por cuenta - centro
    IF chrIndBusMC__t = 'S' THEN
      OPEN curMovCueCen_t;
      FETCH curMovCueCen_t INTO regMoviCuen__t;
      WHILE curMovCueCen_t%FOUND LOOP
        dblDebiLoca__g := dblDebiLoca__g + NVL(regMoviCuen__t.MC_____DEBMONLOC_B, 0);
        dblCredLoca__g := dblCredLoca__g + NVL(regMoviCuen__t.MC_____CREMONLOC_B, 0);
        dblDebiOrig__g := dblDebiOrig__g + NVL(regMoviCuen__t.MC_____DEBMONORI_B, 0);
        dblCredOrig__g := dblCredOrig__g + NVL(regMoviCuen__t.MC_____CREMONORI_B, 0);

        FETCH curMovCueCen_t INTO regMoviCuen__t;
      END LOOP;
    END IF;
  END IF;

  IF chrCodCenUti_p IS NULL AND intIdenTerc__p IS NOT NULL THEN
    -- Saldos por cuenta - tercero
    OPEN curSalCueTer_t;
    FETCH curSalCueTer_t INTO regSaldCuen__t;
    WHILE curSalCueTer_t%FOUND LOOP
      CALCULARSALDOS(regSaldCuen__t);
      FETCH curSalCueTer_t INTO regSaldCuen__t;
    END LOOP;

    -- Movimiento por cuenta - tercero
    IF chrIndBusMC__t = 'S' THEN
      OPEN curMovCueTer_t;
      FETCH curMovCueTer_t INTO regMoviCuen__t;
      WHILE curMovCueTer_t%FOUND LOOP
        dblDebiLoca__g := dblDebiLoca__g + NVL(regMoviCuen__t.MC_____DEBMONLOC_B, 0);
        dblCredLoca__g := dblCredLoca__g + NVL(regMoviCuen__t.MC_____CREMONLOC_B, 0);
        dblDebiOrig__g := dblDebiOrig__g + NVL(regMoviCuen__t.MC_____DEBMONORI_B, 0);
        dblCredOrig__g := dblCredOrig__g + NVL(regMoviCuen__t.MC_____CREMONORI_B, 0);

        FETCH curMovCueTer_t INTO regMoviCuen__t;
      END LOOP;
    END IF;
  END IF;

  IF chrCodCenUti_p IS NOT NULL AND intIdenTerc__p IS NOT NULL THEN
    -- Saldos por cuenta - centro - tercero
    OPEN curSaCuCeTe__t;
    FETCH curSaCuCeTe__t INTO regSaldCuen__t;
    WHILE curSaCuCeTe__t%FOUND LOOP
      CALCULARSALDOS(regSaldCuen__t);
      FETCH curSaCuCeTe__t INTO regSaldCuen__t;
    END LOOP;

    -- Movimiento por cuenta - centro - tercero
    IF chrIndBusMC__t = 'S' THEN
      OPEN curMoCuCeTe__t;
      FETCH curMoCuCeTe__t INTO regMoviCuen__t;
      WHILE curMoCuCeTe__t%FOUND LOOP
        dblDebiLoca__g := dblDebiLoca__g + NVL(regMoviCuen__t.MC_____DEBMONLOC_B, 0);
        dblCredLoca__g := dblCredLoca__g + NVL(regMoviCuen__t.MC_____CREMONLOC_B, 0);
        dblDebiOrig__g := dblDebiOrig__g + NVL(regMoviCuen__t.MC_____DEBMONORI_B, 0);
        dblCredOrig__g := dblCredOrig__g + NVL(regMoviCuen__t.MC_____CREMONORI_B, 0);

        FETCH curMoCuCeTe__t INTO regMoviCuen__t;
      END LOOP;
    END IF;
  END IF;

  -- Calcula los saldos finales de acuerdo a la naturaleza de la cuenta
  dblSalFinLoc_g := dblSalIniLoc_g + intNatuCuen__t * (dblDebiLoca__g - dblCredLoca__g);
  dblSalFinOri_g := dblSalIniOri_g + intNatuCuen__t * (dblDebiOrig__g - dblCredOrig__g);

  IF (dblDebiLoca__p <> 0) OR (dblCredLoca__p <> 0) THEN
    chrIndHijSal_g := 'S';
  END IF;  

  dblSalIniLoc_p := dblSalIniLoc_g;
  dblSalIniOri_p := dblSalIniOri_g;
  dblDebiLoca__p := dblDebiLoca__g;
  dblCredLoca__p := dblCredLoca__g;
  dblDebiOrig__p := dblDebiOrig__g;
  dblCredOrig__p := dblCredOrig__g;
  dblSalFinLoc_p := dblSalFinLoc_g;
  dblSalFinOri_p := dblSalFinOri_g;
  chrIndHijSal_p := chrIndHijSal_g;  
 
END CalcularSaldoCuenta;



END Ap_fina_ConSaldosAllianzPkg;
/
