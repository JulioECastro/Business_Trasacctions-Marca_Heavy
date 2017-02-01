CREATE OR REPLACE PACKAGE GBU_ODS_CALCULOS_PKG
IS
/*********************************************************************************************
Author  :   Federico Guillemet (t52541)
Created :   2008-02-29
Purpose :

Historial
Date            Person				Description
------------    ------------------	-------------------------------------
2008-02-29	t52541			Creacion del Paquete

*********************************************************************************************/

-- Public type declarations

-- Public constant declarations

-- Public variable declarations

-- Public Exception

-- Public function and procedure declarations

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   calculos_fn
	Sistema: GBU
	Objetivo: Obtiene una cadena de proceso a partir de un PEP de entrada.


	Parámetros de entrada:
		p_c_branch_oi_id: Identificador de la Business Unit a la cual pertence la SubBU a asignar.
		p_origen: Representa el segmento relacionado a la operacion.



	Parámetros de salida:
		Salida de Funcion: retorna una cadena de proceso

	Notas:
	Autor: Federico Guillemet
	Historia:
	Fecha		Autor		Descripción
	2008-02-29	t52541		Creacion de procedimiento
        2011-07-06	T53605		Modificado
        2011-07-18	T53605		Modificado, se agrego logica al proceso de cadena cuando la OI de entrada es DAL se debe buscar por
        				DOC_TYPE_CODE o DOCUMENT_MONTH
        2011-07-25	T53605		Modificado, se agrego logica correspondiente Rama Significativa de un arbol extraido de la ODS_BUSINESS_TRANSACTIONS

*********************************************************************************************/
FUNCTION SO_ANTERIORES_NEW_FN(
         P_C_BRANCH_OI_ID    IN GBU_FT_FACTURACION.C_BRANCH_OI_ID%TYPE
        ,P_FAMILY_TYPE_REF   IN ODS_BUSN_TRANSACTIONS_PEP_MV.FAMILY_TYPE_REF%TYPE  DEFAULT NULL
        ,P_WITH_88           IN ODS_COMMON_PKG.S_STR_BOOLEAN                       DEFAULT ODS_COMMON_PKG.C_STR_FALSE)
RETURN GBU_CADENA_TABLE_TYPE;

FUNCTION SO_ANTERIORES_NEW_FN(
          P_BUSINESS_ID            IN ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_ID%TYPE
         ,P_BUSINESS_SEQUENCE      IN ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_SEQUENCE%TYPE
         ,P_FAMILY_TYPE_REF        IN ODS_BUSN_TRANSACTIONS_PEP_MV.FAMILY_TYPE_REF%TYPE		 
         ,P_DOC_COMM_LOCATION_CODE IN ODS_BUSN_TRANSACTIONS_PEP_MV.DOC_COMM_LOCATION_CODE%TYPE
		     ,P_WITH_88           	   IN ODS_COMMON_PKG.S_STR_BOOLEAN DEFAULT ODS_COMMON_PKG.C_STR_FALSE)
RETURN GBU_CADENA_TABLE_TYPE;

FUNCTION CALCULOS_FN(
         P_C_BRANCH_OI_ID    IN GBU_FT_FACTURACION.C_BRANCH_OI_ID%TYPE
        ,P_ORIGEN            IN VARCHAR2)
RETURN GBU_FT_FACTURACION%ROWTYPE;

END gbu_ods_calculos_pkg;
/
CREATE OR REPLACE PACKAGE BODY GBU_ODS_CALCULOS_PKG
IS
--TIPOS/SUBTIPOS PRIVADOS-------------------------------------------------------
    TYPE tr_po IS RECORD (
        selling_po_comm_loc_code   gbu_facturas.selling_po_comm_loc_code%TYPE,
        selling_po_year            gbu_facturas.selling_po_year%TYPE,
        selling_po_month           gbu_facturas.selling_po_month%TYPE,
        selling_po_doc_type_code   gbu_facturas.selling_po_doc_type_code%TYPE,
        selling_po_number          gbu_facturas.selling_po_number%TYPE,
        selling_po_itm_num         gbu_facturas.selling_po_itm_num%TYPE);


--CONSTANTES PRIVADAS-----------------------------------------------------------
    c_portfolio_type_88 CONSTANT VARCHAR2(2):='88';
    c_doc_port_type     CONSTANT ods_business_transactions.doc_portfolio_type%TYPE := '01';

--VARIABLES PRIVADAS------------------------------------------------------------


-- *****************************************************************************
-- DEFINICION DE MODULOS PRIVADOS
-- *****************************************************************************
--------------------------------------------------------------------------------
/*
Nombre módulo: so_anterior_fn
Objetivo:
Author:   t52541 Federico Guillemet
Version:  [2008-02-29] [t52541] Creado
          [2011-07-06] [t53605] Modificado
          [2011-07-25] [t53605] Modificado
*/
FUNCTION so_anterior_fn(p_business_id            IN ods_business_transactions.business_id%TYPE,
                        p_business_sequence      IN ods_business_transactions.business_sequence%TYPE,
                        p_doc_comm_location      IN ods_business_transactions.doc_comm_location_code%TYPE,
                        p_secondary_costing_flag IN ods_business_transactions.secondary_costing_flag%TYPE)
RETURN VARCHAR2;

--------------------------------------------------------------------------------
/*
Nombre módulo: po_asociada_fn
Objetivo:
Author:   t52541 Federico Guillemet
Version:  [2008-02-29] [t52541] Creado
          [2011-07-25] [t53605] Modificado
*/
FUNCTION po_asociada_fn(
    p_business_id            IN ods_business_transactions.business_id%TYPE,
    p_business_sequence      IN ods_business_transactions.business_sequence%TYPE,
    p_secondary_costing_flag IN ods_business_transactions.secondary_costing_flag%TYPE)
RETURN VARCHAR2;

--------------------------------------------------------------------------------
/*
Nombre módulo: po_asociada_fn
Objetivo:
Author:   t52541 Federico Guillemet
Version:  [2008-02-29] [t52541] Creado
          [2011-07-06] [t53605] Modificado
          [2011-07-25] [t53605] Modificado
*/
FUNCTION po_anterior_fn(
                        p_business_id            IN ods_business_transactions.business_id%TYPE,
                        p_business_sequence      IN ods_business_transactions.business_sequence%TYPE,
                        p_doc_comm_location      IN ods_business_transactions.doc_comm_location_code%TYPE,
                        p_secondary_costing_flag IN ods_business_transactions.secondary_costing_flag%TYPE)
RETURN tr_po;

--------------------------------------------------------------------------------
/*
Nombre módulo: mill_valor_fn
Objetivo:
Author:   t52541 Federico Guillemet
Version:  [2008-02-29] [t52541] Creado
	  [2011-07-25] [t53605] Modificado
*/
FUNCTION mill_valor_fn(
    p_business_id            IN ods_business_transactions.business_id%TYPE,
    p_business_sequence      IN ods_business_transactions.business_sequence%TYPE,
    p_secondary_costing_flag IN ods_business_transactions.secondary_costing_flag%TYPE)
RETURN VARCHAR2;

-- *****************************************************************************
-- IMPLEMENTACION DE MODULOS PRIVADOS
-- *****************************************************************************
FUNCTION so_anterior_fn(
                        p_business_id            IN ods_business_transactions.business_id%TYPE,
                        p_business_sequence      IN ods_business_transactions.business_sequence%TYPE,
                        p_doc_comm_location      IN ods_business_transactions.doc_comm_location_code%TYPE,
                        p_secondary_costing_flag IN ods_business_transactions.secondary_costing_flag%TYPE
                       )
RETURN VARCHAR2 IS
    --CURSORES
    --
    CURSOR cr_obtener_documento_unico(pc_business_sequence      IN ods_business_transactions.business_sequence%TYPE,
                                      pc_business_id            IN ods_business_transactions.business_id%TYPE,
                                      pc_doc_comm_location      IN ods_business_transactions.doc_comm_location_code%TYPE,
                                      pc_secondary_costing_flag IN ods_business_transactions.secondary_costing_flag%TYPE)
    IS
    SELECT  doc_comm_location_code,
            document_year,
            RPAD(doc_type_code,2,' '),
            document_number,
            document_item_num
    FROM    ods_business_transactions
    WHERE   business_id = pc_business_id
    AND     doc_portfolio_type = '02'
    AND     business_sequence > pc_business_sequence
    AND     doc_comm_location_code <> pc_doc_comm_location
    AND     secondary_costing_flag = pc_secondary_costing_flag
    AND     historical_link_flag = 'N'
    ORDER BY business_sequence;
    --
    --Retorna set de documentos no referenciados, cuyo sequence sea menor al ingresado por parametro
    CURSOR cr_obtener_documento_noref(pc_business_sequence      IN ods_business_transactions.business_sequence%TYPE,
                                      pc_business_id            IN ods_business_transactions.business_id%TYPE,
                                      pc_doc_comm_location      IN ods_business_transactions.doc_comm_location_code%TYPE,
                                      pc_secondary_costing_flag IN ods_business_transactions.secondary_costing_flag%TYPE)
    IS
    SELECT  doc_comm_location_code,
            document_year,
            RPAD(doc_type_code,2,' '),
            document_number,
            document_item_num
    FROM    ods_business_transactions
    WHERE   business_id = pc_business_id
    AND     doc_portfolio_type = '02'
    AND     business_sequence > pc_business_sequence
    AND     business_reference_id IS NULL
    AND     doc_comm_location_code <> pc_doc_comm_location
    AND     secondary_costing_flag = pc_secondary_costing_flag
    AND     historical_link_flag = 'N'
    ORDER BY business_sequence;
    --
    --Retorna set de registros con mas de un documento referenciado
    CURSOR cr_obtener_documento_mayor_seq(pc_business_sequence      IN ods_business_transactions.business_sequence%TYPE,
                                          pc_business_id            IN ods_business_transactions.business_id%TYPE,
                                          pc_doc_comm_location      IN ods_business_transactions.doc_comm_location_code%TYPE,
                                          pc_secondary_costing_flag IN ods_business_transactions.secondary_costing_flag%TYPE)
    IS
    SELECT  doc_comm_location_code,
            document_year,
            RPAD(doc_type_code,2,' '),
            document_number,
            document_item_num
    FROM    ods_business_transactions
    WHERE   business_id = pc_business_id
    AND     doc_portfolio_type = '02'
    AND     business_sequence > pc_business_sequence
    AND     doc_comm_location_code <> pc_doc_comm_location
    AND     secondary_costing_flag = pc_secondary_costing_flag
    AND     historical_link_flag = 'N'
    ORDER BY business_reference_id DESC,
             business_reference_seq;
    --
    v_c_so_id                    gbu_ft_facturacion.c_branch_oi_ant_id%TYPE;

    v_doc_comm_location          ods_business_transactions.doc_comm_location_code%TYPE;
    v_business_id                ods_business_transactions.business_id%TYPE;
    v_business_sequence          ods_business_transactions.business_sequence%TYPE;
    v_secondary_costing_flag     ods_business_transactions.secondary_costing_flag%TYPE;

    v_dat_c_branch_id            ods_business_transactions.doc_comm_location_code%TYPE;
    v_dat_c_year_doc             ods_business_transactions.document_year%TYPE;
    v_dat_c_type_doc             ods_business_transactions.doc_type_code%TYPE;
    v_dat_c_doc                  ods_business_transactions.document_number%TYPE;
    v_dat_c_item_doc             ods_business_transactions.document_item_num%TYPE;

    v_cantnorefe                 PLS_INTEGER;
    v_cantregistros              PLS_INTEGER;

BEGIN
     --
     v_cantregistros       := 0;
     v_c_so_id            := 'UND-00000000000000-00000';
     --
     v_business_id        := p_business_ID;
     v_business_sequence  := p_business_sequence;
     v_doc_comm_location  := p_doc_comm_location;
     v_secondary_costing_flag := p_secondary_costing_flag;
     --
     /*Busca la cantidad de documentos*/
     SELECT COUNT(1)
     INTO   v_CantRegistros
     FROM   ods_business_transactions
     WHERE  business_id = v_business_id
     AND    doc_portfolio_type = '02'
     AND    business_sequence > v_business_sequence
     AND    doc_comm_location_code <> v_doc_comm_location
     AND    secondary_costing_flag = v_secondary_costing_flag
     AND    historical_link_flag = 'N';
     --
     IF v_CantRegistros <= 1 THEN
        --
        IF v_CantRegistros = 1 THEN
           --
           OPEN  cr_obtener_documento_unico(v_business_sequence,v_business_id,v_doc_comm_location,v_secondary_costing_flag);
           FETCH cr_obtener_documento_unico INTO v_dat_c_branch_id, v_dat_c_year_doc, v_dat_c_type_doc, v_dat_c_doc, v_dat_c_item_doc;
           CLOSE cr_obtener_documento_unico;
           --
           IF v_dat_c_branch_id IS NOT NULL AND v_dat_c_year_doc IS NOT NULL AND v_dat_c_type_doc IS NOT NULL AND v_dat_c_doc IS NOT NULL AND
              v_dat_c_item_doc IS NOT NULL THEN
              --
              --Arma el PEP de la SO anterior
              v_c_so_id := v_dat_c_branch_id || '-'  || LPAD(v_dat_c_year_doc,4,0) || RPAD(v_dat_c_type_doc,2,' ') || LPAD(v_dat_c_doc,8,0) || '-' || LPAD(v_dat_c_item_doc,5,0);
              --
           END IF;
           --
        END IF;
        --
     ELSE
        --Si la cantidad de documentos es mayor a 1 entonces:
        --Busca la cantidad de documentos no referenciados
        SELECT COUNT(1)
        INTO   v_CantNoRefe
        FROM   ods_business_transactions
        WHERE  business_id = v_business_id
        AND    doc_portfolio_type ='02'
        AND    business_sequence > v_business_sequence
        AND    business_reference_id IS NULL
        AND    doc_comm_location_code <> v_doc_comm_location
        AND    secondary_costing_flag = v_secondary_costing_flag
        AND    historical_link_flag = 'N';
        --
        IF v_CantNoRefe >=1 THEN
            /*Hay documentos no referenciados, devuelve el de menor business_sequence*/
            OPEN  cr_obtener_documento_noref(v_business_sequence,v_business_id,v_doc_comm_location,v_secondary_costing_flag);
            FETCH cr_obtener_documento_noref INTO v_dat_c_branch_id, v_dat_c_year_doc, v_dat_c_type_doc, v_dat_c_doc, v_dat_c_item_doc;
            CLOSE cr_obtener_documento_noref;
            --
            IF v_dat_c_branch_id IS NOT NULL AND v_dat_c_year_doc IS NOT NULL AND v_dat_c_type_doc IS NOT NULL AND  v_dat_c_doc IS NOT NULL AND
               v_dat_c_item_doc IS NOT NULL THEN
               --
               --Arma el PEP de la SO anterior
               v_c_so_id := v_dat_c_branch_id || '-'  || LPAD(v_dat_c_year_doc,4,0) || RPAD(v_dat_c_type_doc,2,' ') || LPAD(v_dat_c_doc,8,0) || '-' || LPAD(v_dat_c_item_doc,5,0);
               --
            END IF;
            --
        ELSE
            --Si la cantidad de documentos NO Ref es cero entonces buscar el documento de mayor secuencia
            --
            OPEN  cr_obtener_documento_mayor_seq(v_business_sequence,v_business_id,v_doc_comm_location,v_secondary_costing_flag);
            FETCH cr_obtener_documento_mayor_seq INTO v_dat_c_branch_id, v_dat_c_year_doc, v_dat_c_type_doc, v_dat_c_doc, v_dat_c_item_doc;
            CLOSE cr_obtener_documento_mayor_seq;
            --
            IF v_dat_c_branch_id IS NOT NULL AND v_dat_c_year_doc IS NOT NULL AND v_dat_c_type_doc IS NOT NULL AND  v_dat_c_doc IS NOT NULL AND
               v_dat_c_item_doc IS NOT NULL THEN
               --
               --Arma el PEP de la SO anterior
               v_c_so_id := v_dat_c_branch_id || '-'  || LPAD(v_dat_c_year_doc,4,0) || RPAD(v_dat_c_type_doc,2,' ') || LPAD(v_dat_c_doc,8,0) || '-' || LPAD(v_dat_c_item_doc,5,0);
               --
            END IF;
            --
        END IF;
        --
     END IF;
     --
     RETURN NVL(v_c_so_id,'UND-00000000000000-00000');
     --
END so_anterior_fn;
------------------------------------------------------------------------------------------------------
FUNCTION po_asociada_fn (
                         p_business_id              IN ods_business_transactions.business_id%TYPE,
                         p_business_sequence        IN ods_business_transactions.business_sequence%TYPE,
                         p_secondary_costing_flag   IN ods_business_transactions.secondary_costing_flag%TYPE
)
   RETURN VARCHAR2
IS
   CURSOR cr_busca_doc_anterior (
                                 pc_business_sequence       IN  ods_business_transactions.business_sequence%TYPE,
                                 pc_business_id             IN  ods_business_transactions.business_id%TYPE,
                                 pc_secondary_costing_flag  IN 	ods_business_transactions.secondary_costing_flag%TYPE
   )
   IS
      SELECT   t.business_id,
               t.business_sequence,
               t.doc_comm_location_code,
               t.document_year,
               RPAD (t.doc_type_code, 2, ' '),
               t.document_number,
               t.document_item_num,
               t.doc_type_code,
               t.doc_comm_location_code,
               t.secondary_costing_flag
      FROM     ods_business_transactions t
      WHERE    t.business_id = pc_business_id
      AND      t.business_sequence < pc_business_sequence
      AND      t.doc_portfolio_type = c_doc_port_type
      AND      t.secondary_costing_flag = p_secondary_costing_flag
      AND      t.historical_link_flag = 'N'
      ORDER BY t.business_sequence DESC;

   CURSOR cr_busca_doct (
                         pc_business_sequence       IN ods_business_transactions.business_sequence%TYPE,
                         pc_business_id             IN ods_business_transactions.business_id%TYPE,
                         pc_secondary_costing_flag  IN ods_business_transactions.secondary_costing_flag%TYPE
   )
   IS
      SELECT DECODE ('G01',
                     t.doc_comm_location_code, 'SID',
                     t.doc_comm_location_code
                    ),
             t.document_year,
             RPAD (t.doc_type_code, 2, ' '),
             t.document_number,
             t.document_item_num,
             t.doc_portfolio_type
      FROM   ods_business_transactions t
      WHERE  t.business_id = pc_business_id
      AND    t.business_sequence = pc_business_sequence - 1
      AND    t.doc_portfolio_type = c_portfolio_type_88
      AND    t.secondary_costing_flag = pc_secondary_costing_flag
      AND    t.historical_link_flag = 'N';

   v_ant_business_id             ods_business_transactions.business_id%TYPE;
   v_ant_business_sequence       ods_business_transactions.business_sequence%TYPE;
   v_ant_c_branch_id             ods_business_transactions.doc_comm_location_code%TYPE;
   v_ant_c_year_doc              ods_business_transactions.document_year%TYPE;
   v_ant_c_type_doc              ods_business_transactions.doc_type_code%TYPE;
   v_ant_c_doc                   ods_business_transactions.document_number%TYPE;
   v_ant_c_item_doc              ods_business_transactions.document_item_num%TYPE;
   v_ant_c_doc_type_code         ods_business_transactions.doc_type_code%TYPE;
   v_ant_doc_comm_location       ods_business_transactions.doc_comm_location_code%TYPE;
   v_ant_secondary_costing_flag  ods_business_transactions.secondary_costing_flag%TYPE;
   v_dat_c_branch_id             ods_business_transactions.doc_comm_location_code%TYPE;
   v_dat_c_year_doc              ods_business_transactions.document_year%TYPE;
   v_dat_c_type_doc              ods_business_transactions.doc_type_code%TYPE;
   v_dat_c_doc                   ods_business_transactions.document_number%TYPE;
   v_dat_c_item_doc              ods_business_transactions.document_item_num%TYPE;
   v_dat_doc_portfolio_type      ods_business_transactions.doc_portfolio_type%TYPE;
   v_c_po_id                     gbu_ft_facturacion.c_po_id%TYPE;
BEGIN
   v_c_po_id := 'UND-00000000000000-00000';

   --Busca el documento anterior al doc encontrado
   OPEN cr_busca_doc_anterior (p_business_sequence, p_business_id, p_secondary_costing_flag);
   FETCH cr_busca_doc_anterior INTO v_ant_business_id,
                                    v_ant_business_sequence,
                                    v_ant_c_branch_id,
                                    v_ant_c_year_doc,
                                    v_ant_c_type_doc,
                                    v_ant_c_doc,
                                    v_ant_c_item_doc,
                                    v_ant_c_doc_type_code,
                                    v_ant_doc_comm_location,
                                    v_ant_secondary_costing_flag;

   IF v_ant_business_id IS NOT NULL AND v_ant_business_sequence IS NOT NULL THEN
      --
      IF v_ant_c_doc_type_code = 'IM' THEN
         --
         IF v_ant_doc_comm_location IN ('TAM', 'ALG', 'TAV', 'G01') THEN
            --
            OPEN cr_busca_doct (v_ant_business_sequence, v_ant_business_id, v_ant_secondary_costing_flag);
            FETCH cr_busca_doct INTO  v_dat_c_branch_id,
                                      v_dat_c_year_doc,
                                      v_dat_c_type_doc,
                                      v_dat_c_doc,
                                      v_dat_c_item_doc,
                                      v_dat_doc_portfolio_type;
	    --
            IF v_dat_doc_portfolio_type = c_portfolio_type_88 THEN
               --
               v_c_po_id := v_dat_c_branch_id || '-' || LPAD (v_dat_c_year_doc, 4, 0) || RPAD (v_dat_c_type_doc, 2, ' ') ||
                            LPAD (v_dat_c_doc, 8, 0) || '-' || LPAD (v_dat_c_item_doc, 5, 0);
               --
            END IF;
	    --
            CLOSE cr_busca_doct;
            --
         ELSE
            --
            v_c_po_id := v_ant_c_branch_id || '-' || LPAD (v_ant_c_year_doc, 4, 0) || RPAD (v_ant_c_type_doc, 2, ' ') || LPAD (v_ant_c_doc, 8, 0) ||
                         '-' || LPAD (v_ant_c_item_doc, 5, 0);
            --
         END IF;
         --
      ELSE
         --
         v_c_po_id := v_ant_c_branch_id || '-' || LPAD (v_ant_c_year_doc, 4, 0) || RPAD (v_ant_c_type_doc, 2, ' ') || LPAD (v_ant_c_doc, 8, 0) ||
                      '-' || LPAD (v_ant_c_item_doc, 5, 0);
         --
      END IF;
      --
   END IF;
   --
   CLOSE cr_busca_doc_anterior;
   --
   RETURN (v_c_po_id);
   --
END po_asociada_fn;

--------------------------------------------------------------------------------
FUNCTION po_anterior_fn(
                        p_business_id       	   IN ods_business_transactions.business_id%TYPE,
                        p_business_sequence 	   IN ods_business_transactions.business_sequence%TYPE,
                        p_doc_comm_location 	   IN ods_business_transactions.doc_comm_location_code%TYPE,
                        p_secondary_costing_flag   IN ods_business_transactions.secondary_costing_flag%TYPE
                        )
RETURN tr_po IS
    --CURSORES
    --
    CURSOR cur_obtbranchoiantidunico(p_cur_business_id          IN ods_business_transactions.business_id%TYPE,
                                     p_cur_business_sequence    IN ods_business_transactions.business_sequence%TYPE,
                                     p_cur_doc_comm_location    IN ods_business_transactions.doc_comm_location_code%TYPE,
                                     p_secondary_costing_flag   IN ods_business_transactions.secondary_costing_flag%TYPE)
    IS
    SELECT  doc_comm_location_code,
            document_year,
            document_month,
            RPAD(doc_type_code, 2, ' ') AS doc_type_code,
            document_number,
            document_item_num
    FROM    ods_business_transactions
    WHERE   business_id        = p_cur_business_id
    AND     doc_portfolio_type = c_doc_port_type
    AND     business_sequence  > p_cur_business_sequence
    AND     doc_comm_location_code = p_cur_doc_comm_location
    AND	    secondary_costing_flag = p_secondary_costing_flag
    AND     historical_link_flag = 'N'
    ORDER BY business_sequence;
    --
    --Retorna set de documentos no referenciados, cuyo sequence sea menor al ingresado por parametro
    CURSOR cur_obtbranchoiantid(p_cur_business_id          IN ods_business_transactions.business_id%TYPE,
                                p_cur_business_sequence    IN ods_business_transactions.business_sequence%TYPE,
                                p_cur_doc_comm_location    IN ods_business_transactions.doc_comm_location_code%TYPE,
                                p_secondary_costing_flag   IN ods_business_transactions.secondary_costing_flag%TYPE)
    IS
    SELECT  doc_comm_location_code,
            document_year,
            document_month,
            RPAD(doc_type_code, 2, ' ') AS doc_type_code,
            document_number,
            document_item_num
    FROM    ods_business_transactions
    WHERE   business_id        = p_cur_business_id
    AND     doc_portfolio_type = c_doc_port_type
    AND     business_sequence  > p_cur_business_sequence
    AND     business_reference_id IS NULL
    AND     doc_comm_location_code = p_cur_doc_comm_location
    AND	    secondary_costing_flag = p_secondary_costing_flag
    AND     historical_link_flag = 'N'
    ORDER BY business_sequence;
    --
    --Retorna set de registros con mas de un documento referenciado
    CURSOR cur_obtbranchoiantidmayor(p_cur_business_id          IN ods_business_transactions.business_id%TYPE,
                                     p_cur_business_sequence    IN ods_business_transactions.business_sequence%TYPE,
                                     p_cur_doc_comm_location    IN ods_business_transactions.doc_comm_location_code%TYPE,
                                     p_secondary_costing_flag   IN ods_business_transactions.secondary_costing_flag%TYPE)
    IS
    SELECT  doc_comm_location_code,
            document_year,
            document_month,
            RPAD(doc_type_code, 2, ' ') AS doc_type_code,
            document_number,
            document_item_num
    FROM    ods_business_transactions
    WHERE   business_id        = p_cur_business_id
    AND     doc_portfolio_type = c_doc_port_type
    AND     business_sequence  > p_cur_business_sequence
    AND     doc_comm_location_code = p_cur_doc_comm_location
    AND	    secondary_costing_flag = p_secondary_costing_flag
    AND     historical_link_flag = 'N'
    ORDER BY business_reference_id DESC,
             business_reference_seq;
    --
    v_po                tr_po;
    v_cantnorefe        PLS_INTEGER;
    v_cantregistros     PLS_INTEGER;
    --
BEGIN
    --
    v_cantregistros       := 0;
    --
    SELECT  COUNT(1)
    INTO    v_cantregistros
    FROM    ods_business_transactions
    WHERE   business_id        = p_business_id
    AND     doc_portfolio_type = c_doc_port_type
    AND     business_sequence  > p_business_sequence
    AND     doc_comm_location_code = p_doc_comm_location
    AND	    secondary_costing_flag = p_secondary_costing_flag
    AND     historical_link_flag = 'N';
    --
    IF v_cantregistros <= 1 THEN
        --
        IF v_cantregistros = 1 THEN
            --
            OPEN  cur_obtbranchoiantidunico(p_business_id,p_business_sequence,p_doc_comm_location,p_secondary_costing_flag);
            FETCH cur_obtbranchoiantidunico INTO v_po;
            CLOSE cur_obtbranchoiantidunico;
            --
        END IF;
        --
    ELSE
        /*Busca la cantidad de documentos no referenciados*/
        SELECT  COUNT(1)
        INTO    v_cantnorefe
        FROM    ods_business_transactions
        WHERE   business_id        = p_business_id
        AND     doc_portfolio_type = c_doc_port_type
        AND     business_sequence  > p_business_sequence
        AND     business_reference_id IS NULL
        AND     doc_comm_location_code = p_doc_comm_location
        AND	secondary_costing_flag = p_secondary_costing_flag
        AND     historical_link_flag = 'N';
        --
        IF v_cantnorefe >= 1 THEN
            --
            /*Hay documentos no referenciados, devuelve el de menor business_sequence*/
            OPEN  cur_obtbranchoiantid(p_business_id,p_business_sequence,p_doc_comm_location,p_secondary_costing_flag);
            FETCH cur_obtbranchoiantid INTO v_po;
            CLOSE cur_obtbranchoiantid;
            --
        ELSE
            --
            OPEN  cur_obtbranchoiantidmayor(p_business_id,p_business_sequence,p_doc_comm_location,p_secondary_costing_flag);
            FETCH cur_obtbranchoiantidmayor INTO v_po;
            CLOSE cur_obtbranchoiantidmayor;
            --
        END IF;
        --
    END IF;
    --
    RETURN v_po;
    --
END po_anterior_fn;
-----------------------------
FUNCTION mill_valor_fn (
                        p_business_id              IN       ods_business_transactions.business_id%TYPE,
                        p_business_sequence        IN       ods_business_transactions.business_sequence%TYPE,
                        p_secondary_costing_flag   IN       ods_business_transactions.secondary_costing_flag%TYPE
)
   RETURN VARCHAR2
IS
   v_c_mill_oi_id                gbu_ft_facturacion.c_mill_oi_id%TYPE;
   v_business_id                 ods_business_transactions.business_id%TYPE;
   v_business_sequence           ods_business_transactions.business_sequence%TYPE;
   --reg_bus_tra                   ods_business_transactions%ROWTYPE;
   --v_cant_oo                     PLS_INTEGER;
   v_cant_im                     PLS_INTEGER;
   v_cant_noref                  PLS_INTEGER;
   v_cant_bs                     PLS_INTEGER;
   --v_cant_tiene_bus_seq          PLS_INTEGER;
   --v_cant_intermil               PLS_INTEGER;
   --v_tiene_im                    BOOLEAN;
   --v_business_id_im              ods_business_transactions.business_id%TYPE;
   --v_business_sequence_im        ods_business_transactions.business_sequence%TYPE;
   --v_doc_type_code_im            ods_business_transactions.doc_type_code%TYPE;
   v_doc_comm_location_code_im   ods_business_transactions.doc_comm_location_code%TYPE;
   v_doc_portfolio_type_im       ods_business_transactions.doc_portfolio_type%TYPE;
   v_doc_comm_location_code_ret  ods_business_transactions.doc_comm_location_code%TYPE;
   v_document_year_ret           ods_business_transactions.document_year%TYPE;
   v_document_month_ret          ods_business_transactions.document_month%TYPE;
   v_doc_type_code_ret           ods_business_transactions.doc_type_code%TYPE;
   v_document_number_ret         ods_business_transactions.document_number%TYPE;
   v_document_item_num_ret       ods_business_transactions.document_item_num%TYPE;
   v_doc_comm_location_code_aux  ods_business_transactions.doc_comm_location_code%TYPE;
   v_secondary_costing_flag      ods_business_transactions.secondary_costing_flag%TYPE;

   CURSOR cr_busca_bs
   IS
      SELECT COUNT (DISTINCT b.business_sequence)
      FROM   ods_business_transactions b
      WHERE  b.business_id = v_business_id
      AND    b.business_sequence > v_business_sequence
      AND    b.doc_portfolio_type = c_portfolio_type_88
      AND    b.secondary_costing_flag = v_secondary_costing_flag
      AND    b.historical_link_flag = 'N';

   CURSOR cr_soc_im_bs
   IS
      SELECT   COUNT (DISTINCT bu.business_sequence)
      FROM     ods_business_transactions bu
      WHERE    bu.business_id = v_business_id
      AND      bu.business_sequence > v_business_sequence
      AND      bu.doc_portfolio_type = c_portfolio_type_88
      AND      bu.secondary_costing_flag = v_secondary_costing_flag
      AND      bu.historical_link_flag = 'N'
      AND      NOT EXISTS (
                  SELECT 1
                  FROM   ods_business_transactions bu2
                  WHERE  bu2.business_id = v_business_id
                  AND    bu2.doc_type_code = 'IM'
                  AND    REPLACE (bu.doc_comm_location_code, 'TYP', 'G01') = REPLACE (bu2.doc_comm_location_code, 'TYP', 'G01')
                  AND    bu2.secondary_costing_flag = v_secondary_costing_flag
                  AND    bu2.historical_link_flag = 'N')
      ORDER BY bu.business_sequence DESC;

   CURSOR cr_busca_noref
   IS
      SELECT COUNT (DISTINCT b.business_sequence)
      FROM   ods_business_transactions b
      WHERE  b.business_id = v_business_id
      AND    b.business_sequence > v_business_sequence
      AND    b.doc_portfolio_type = c_portfolio_type_88
      AND    b.business_reference_id IS NULL
      AND    b.secondary_costing_flag = v_secondary_costing_flag
      AND    b.historical_link_flag = 'N';

   CURSOR cr_soc_im_noref
   IS
      SELECT   COUNT (DISTINCT bu.business_sequence)
      FROM     ods_business_transactions bu
      WHERE    bu.business_id = v_business_id
      AND      bu.business_sequence > v_business_sequence
      AND      bu.business_reference_id IS NULL
      AND      bu.doc_portfolio_type = c_portfolio_type_88
      AND      bu.secondary_costing_flag = v_secondary_costing_flag
      AND      bu.historical_link_flag = 'N'
      AND      NOT EXISTS (
                  SELECT 1
                  FROM   ods_business_transactions bu2
                  WHERE  bu2.business_id = v_business_id
                  AND    bu2.doc_type_code = 'IM'
                  AND    REPLACE (bu.doc_comm_location_code, 'TYP', 'G01') = REPLACE (bu2.doc_comm_location_code, 'TYP', 'G01')
                  AND    bu2.secondary_costing_flag = v_secondary_costing_flag
                  AND    bu2.historical_link_flag = 'N')
      ORDER BY bu.business_sequence DESC;

   CURSOR cr_busca_datos_noref
   IS
      SELECT   b.doc_comm_location_code,
               b.document_year,
               b.document_month,
               b.doc_type_code,
               b.document_number,
               b.document_item_num
      FROM     ods_business_transactions b
      WHERE    b.business_id = v_business_id
      AND      b.business_sequence > v_business_sequence
      AND      b.doc_portfolio_type = c_portfolio_type_88
      AND      b.business_reference_id IS NULL
      AND      b.secondary_costing_flag = v_secondary_costing_flag
      AND      b.historical_link_flag = 'N'
      ORDER BY b.business_sequence DESC;

   CURSOR cr_soc_im_datos_noref
   IS
      SELECT   bu.doc_comm_location_code,
               bu.document_year,
               bu.document_month,
               bu.doc_type_code,
               bu.document_number,
               bu.document_item_num
      FROM     ods_business_transactions bu
      WHERE    bu.business_id = v_business_id
      AND      bu.business_sequence > v_business_sequence
      AND      bu.business_reference_id IS NULL
      AND      bu.doc_portfolio_type = c_portfolio_type_88
      AND      bu.secondary_costing_flag = v_secondary_costing_flag
      AND      bu.historical_link_flag = 'N'
      AND      NOT EXISTS (
                  SELECT 1
                  FROM   ods_business_transactions bu2
                  WHERE  bu2.business_id = v_business_id
                  AND    bu2.doc_type_code = 'IM'
                  AND    REPLACE (bu.doc_comm_location_code, 'TYP', 'G01') = REPLACE (bu2.doc_comm_location_code, 'TYP', 'G01')
                  AND    bu2.secondary_costing_flag = v_secondary_costing_flag
                  AND    bu2.historical_link_flag = 'N')
      ORDER BY bu.business_sequence DESC;

   CURSOR cr_busca_datos_ref
   IS
      SELECT   b.doc_comm_location_code,
               b.document_year,
               b.document_month,
               b.doc_type_code,
               b.document_number,
               b.document_item_num
      FROM     ods_business_transactions b
      WHERE    b.business_id = v_business_id
      AND      b.business_sequence > v_business_sequence
      AND      b.doc_portfolio_type = c_portfolio_type_88
      AND      b.secondary_costing_flag = v_secondary_costing_flag
      AND      b.historical_link_flag = 'N'
      ORDER BY b.business_reference_id DESC,
               b.business_reference_seq DESC;

   CURSOR cr_soc_im_datos_ref
   IS
      SELECT   bu.doc_comm_location_code,
               bu.document_year,
               bu.document_month,
               bu.doc_type_code,
               bu.document_number,
               bu.document_item_num
      FROM     ods_business_transactions bu
      WHERE    bu.business_id = v_business_id
      AND      bu.business_sequence > v_business_sequence
      AND      bu.doc_portfolio_type = c_portfolio_type_88
      AND      bu.secondary_costing_flag = v_secondary_costing_flag
      AND      bu.historical_link_flag = 'N'
      AND      NOT EXISTS (
                  SELECT 1
                  FROM   ods_business_transactions bu2
                  WHERE  bu2.business_id = v_business_id
                  AND    bu2.doc_type_code = 'IM'
                  AND    REPLACE (bu.doc_comm_location_code, 'TYP', 'G01') = REPLACE (bu2.doc_comm_location_code, 'TYP', 'G01')
                  AND    bu2.secondary_costing_flag = v_secondary_costing_flag
                  AND    bu2.historical_link_flag = 'N')
      ORDER BY bu.business_reference_id DESC,
               bu.business_reference_seq DESC;

   CURSOR cr_busca_dato_unico
   IS
      SELECT   b.doc_comm_location_code,
               b.document_year,
               b.document_month,
               b.doc_type_code,
               b.document_number,
               b.document_item_num
      FROM     ods_business_transactions b
      WHERE    b.business_id = v_business_id
      AND      b.business_sequence > v_business_sequence
      AND      b.doc_portfolio_type = c_portfolio_type_88
      AND      b.secondary_costing_flag = v_secondary_costing_flag
      AND      b.historical_link_flag = 'N'
      ORDER BY b.business_sequence DESC;

   CURSOR cr_busca_dato_unico_con_im
   IS
      SELECT   bu.doc_comm_location_code,
               bu.document_year,
               bu.document_month,
               bu.doc_type_code,
               bu.document_number,
               bu.document_item_num
      FROM     ods_business_transactions bu
      WHERE    bu.business_id = v_business_id
      AND      bu.business_sequence > v_business_sequence
      AND      bu.doc_portfolio_type = c_portfolio_type_88
      AND      bu.secondary_costing_flag = v_secondary_costing_flag
      AND      bu.historical_link_flag = 'N'
      AND      NOT EXISTS (
                  SELECT 1
                  FROM   ods_business_transactions bu2
                  WHERE  bu2.business_id = v_business_id
                  AND    bu2.doc_type_code = 'IM'
                  AND    REPLACE (bu.doc_comm_location_code, 'TYP', 'G01') = REPLACE (bu2.doc_comm_location_code, 'TYP', 'G01')
                  AND    bu2.secondary_costing_flag = v_secondary_costing_flag
                  AND    bu2.historical_link_flag = 'N')
      ORDER BY bu.business_sequence DESC;

   CURSOR cr_bus_doc_portfolio_type
   IS
      SELECT o.doc_portfolio_type,
             o.doc_comm_location_code
      FROM   ods_business_transactions o
      WHERE  o.business_id = v_business_id
      AND    o.business_sequence = v_business_sequence
      AND    o.secondary_costing_flag = v_secondary_costing_flag
      AND    o.historical_link_flag = 'N';

   CURSOR cr_busca_datos
   IS
      SELECT b.doc_comm_location_code,
             b.document_year,
             b.document_month,
             b.doc_type_code,
             b.document_number,
             b.document_item_num
      FROM   ods_business_transactions b
      WHERE  b.business_id = v_business_id
      AND    b.business_sequence = v_business_sequence
      AND    b.secondary_costing_flag = v_secondary_costing_flag
      AND    b.historical_link_flag = 'N';

   CURSOR cr_bus_soc_im
   IS
      SELECT b.doc_comm_location_code
      FROM   ods_business_transactions b
      WHERE  b.business_id = v_business_id
      AND    b.doc_type_code = 'IM'
      AND    b.secondary_costing_flag = v_secondary_costing_flag
      AND    b.historical_link_flag = 'N';
BEGIN
   --
   v_c_mill_oi_id := 'UND-00000000000000-00000';
   --v_tiene_im := FALSE;
   v_business_id := p_business_id;
   v_business_sequence := p_business_sequence;
   v_secondary_costing_flag := p_secondary_costing_flag;
   --
   SELECT COUNT (1)
   INTO   v_cant_im
   FROM   ods_business_transactions b
   WHERE  b.business_id = v_business_id
   AND    b.doc_type_code = 'IM'
   AND    b.secondary_costing_flag = v_secondary_costing_flag
   AND    b.historical_link_flag = 'N';
   --
   IF v_cant_im >= 1 THEN
      -- Existe un IM
      OPEN cr_soc_im_bs;
      FETCH cr_soc_im_bs INTO v_cant_bs;
      CLOSE cr_soc_im_bs;
      --
   ELSE
      --
      OPEN cr_busca_bs;
      FETCH cr_busca_bs INTO v_cant_bs;
      CLOSE cr_busca_bs;
      --
   END IF;

   IF v_cant_bs > 1 THEN
      -- Hay mas de un business sequence
      IF v_cant_im >= 1 THEN
         -- Hay al menos uno que no este referenciado
         OPEN cr_soc_im_noref;
         FETCH cr_soc_im_noref INTO v_cant_noref;
         CLOSE cr_soc_im_noref;
         --
      ELSE
         -- Hay al menos uno que no este referenciado
         OPEN cr_busca_noref;
         FETCH cr_busca_noref INTO v_cant_noref;
         CLOSE cr_busca_noref;
         --
      END IF;
      --
      IF v_cant_noref >= 1 THEN
         -- Toma el mayor business sequence de los no referenciados
         IF v_cant_im >= 1 THEN
            -- El query busca dentro de la cadena sacando los que tengan la misma soc que 'IM'
            OPEN cr_soc_im_datos_noref;
            FETCH cr_soc_im_datos_noref INTO v_doc_comm_location_code_ret, v_document_year_ret, v_document_month_ret, v_doc_type_code_ret,
                  			     v_document_number_ret, v_document_item_num_ret;
            CLOSE cr_soc_im_datos_noref;
            --
         ELSE
            -- El query busca dentro de toda la cadena
            OPEN cr_busca_datos_noref;
            FETCH cr_busca_datos_noref INTO v_doc_comm_location_code_ret, v_document_year_ret, v_document_month_ret, v_doc_type_code_ret,
                  			    v_document_number_ret, v_document_item_num_ret;
            CLOSE cr_busca_datos_noref;
            --
         END IF;
         --
      ELSE
         -- Toma el mayor business_reference, business_reference_seq de los que esten referenciados
         IF v_cant_im >= 1 THEN
            -- El query busca dentro de la cadena sacando los que tengan la misma soc que 'IM'
            OPEN cr_soc_im_datos_ref;
            FETCH cr_soc_im_datos_ref INTO v_doc_comm_location_code_ret, v_document_year_ret, v_document_month_ret, v_doc_type_code_ret,
                  			   v_document_number_ret, v_document_item_num_ret;
            CLOSE cr_soc_im_datos_ref;
            --
         ELSE
            -- El query busca dentro de toda la cadena
            OPEN cr_busca_datos_ref;
            FETCH cr_busca_datos_ref INTO v_doc_comm_location_code_ret, v_document_year_ret, v_document_month_ret, v_doc_type_code_ret,
                  		          v_document_number_ret, v_document_item_num_ret;
            CLOSE cr_busca_datos_ref;
            --
         END IF;
         --
      END IF;
      --
   ELSE
      -- Evalua si se encontro un registro o ninguno
      IF v_cant_bs <> 0 THEN
         --
         IF v_cant_im >= 1 THEN
            -- Devuelve el unico registro encontrado con IM
            OPEN cr_busca_dato_unico_con_im;
            FETCH cr_busca_dato_unico_con_im INTO v_doc_comm_location_code_ret, v_document_year_ret, v_document_month_ret, v_doc_type_code_ret,
                  			          v_document_number_ret, v_document_item_num_ret;
            CLOSE cr_busca_dato_unico_con_im;
            --
         ELSE
            -- Devuelve el unico registro encontrado sin IM
            OPEN cr_busca_dato_unico;
            FETCH cr_busca_dato_unico INTO v_doc_comm_location_code_ret, v_document_year_ret, v_document_month_ret, v_doc_type_code_ret,
                  			   v_document_number_ret, v_document_item_num_ret;
            CLOSE cr_busca_dato_unico;
            --
         END IF;
         --
      ELSE
         -- Traigo datos de la clave de ingreso
         OPEN cr_bus_doc_portfolio_type;
         FETCH cr_bus_doc_portfolio_type INTO v_doc_portfolio_type_im, v_doc_comm_location_code_im;
         CLOSE cr_bus_doc_portfolio_type;
	 --
         IF v_doc_portfolio_type_im = c_portfolio_type_88 THEN
            -- Traigo datos del IM de la cadena
            SELECT COUNT (1)
            INTO   v_cant_im
            FROM   ods_business_transactions b
            WHERE  b.business_id = v_business_id
            AND    b.doc_type_code = 'IM'
            AND    b.secondary_costing_flag = v_secondary_costing_flag
            AND    b.historical_link_flag = 'N';
	    --
            IF v_cant_im >= 1 THEN
               -- Compara la sociedad de la clave de ingreso con la sociedad del 'IM'
               OPEN cr_bus_soc_im;
               FETCH cr_bus_soc_im INTO v_doc_comm_location_code_aux;
               CLOSE cr_bus_soc_im;
	       --
               IF REPLACE (v_doc_comm_location_code_im, 'TYP', 'G01') <> REPLACE (v_doc_comm_location_code_aux, 'TYP', 'G01') THEN
                  -- Toma la clave de ingreso como valor para devolver
                  OPEN cr_busca_datos;
                  FETCH cr_busca_datos INTO v_doc_comm_location_code_ret, v_document_year_ret, v_document_month_ret, v_doc_type_code_ret,
                        		    v_document_number_ret, v_document_item_num_ret;
                  CLOSE cr_busca_datos;
                  --
               END IF;
               --
            ELSE
               -- Toma la clave de ingreso como valor para devolver
               OPEN cr_busca_datos;
               FETCH cr_busca_datos INTO v_doc_comm_location_code_ret, v_document_year_ret, v_document_month_ret, v_doc_type_code_ret,
                     			 v_document_number_ret, v_document_item_num_ret;
               CLOSE cr_busca_datos;
               --
            END IF;
            --
         END IF;
         --
      END IF;
      --
   END IF;
   --
   IF v_doc_comm_location_code_ret IS NOT NULL
      OR v_document_year_ret IS NOT NULL
      OR v_document_month_ret IS NOT NULL
      OR v_doc_type_code_ret IS NOT NULL
      OR v_document_number_ret IS NOT NULL
      OR v_document_item_num_ret IS NOT NULL THEN
      --
      IF v_doc_comm_location_code_ret = 'H10' THEN
      	 --
         v_doc_comm_location_code_ret := 'DAL';
         --
      END IF;
      --
      IF v_doc_comm_location_code_ret = 'G01' THEN
         --
         v_doc_comm_location_code_ret := 'SID';
         --
      END IF;
      --
      IF v_doc_comm_location_code_ret = 'R10' THEN
         --
         v_doc_comm_location_code_ret := 'SIL';
         --
      END IF;
      --
      IF v_doc_comm_location_code_ret = 'G17' THEN
         --
         v_doc_comm_location_code_ret := 'MTM';
         --
      END IF;
      --
      IF v_doc_comm_location_code_ret = 'DAL' OR v_doc_comm_location_code_ret = 'SIL' THEN
      	 --
         v_c_mill_oi_id := v_doc_comm_location_code_ret || '-' || v_document_year_ret || RPAD (v_document_month_ret, 2, ' ') ||
         		   v_document_number_ret || '-' || LPAD (v_document_item_num_ret, 5, '0');
         --
      ELSE
         --
         v_c_mill_oi_id := v_doc_comm_location_code_ret || '-' || v_document_year_ret || RPAD (v_doc_type_code_ret, 2, ' ') ||
            		   v_document_number_ret || '-' || LPAD (v_document_item_num_ret, 5, '0');
         --
      END IF;
      --
   END IF;
   --
   RETURN (v_c_mill_oi_id);
   --
END mill_valor_fn;

-- *****************************************************************************
-- IMPLEMENTACION DE MODULOS PUBLICOS
-- *****************************************************************************
--------------------------------------------------------------------------------
FUNCTION calculos_old_fn(
                     p_c_branch_oi_id    IN gbu_ft_facturacion.c_branch_oi_id%type,
                     p_origen            IN VARCHAR2)
RETURN gbu_ft_facturacion%ROWTYPE IS
    --
    --Busca si existe una cadena con 88 para el documento
    CURSOR buscacadenaochoocho(pc_doc_comm_location_code        ods_business_transactions.doc_comm_location_code%TYPE,
                               pc_document_year                 ods_business_transactions.document_year%TYPE,
                               pc_doc_type_code                 ods_business_transactions.doc_type_code%TYPE,
                               pc_document_number               ods_business_transactions.document_number%TYPE,
                               pc_document_item_num             ods_business_transactions.document_item_num%TYPE)
    IS
    SELECT  COUNT(1)
    FROM    ods_business_transactions t
    WHERE   doc_comm_location_code = pc_doc_comm_location_code
    AND     document_year          = pc_document_year
    AND     TRIM(doc_type_code)    = pc_doc_type_code
    AND     document_number        = pc_document_number
    AND     document_item_num      = pc_document_item_num
    AND     business_reference_id  IS NULL
    AND     business_reference_seq IS NULL
    AND	    historical_link_flag = 'N'
    AND EXISTS
            (
            SELECT  business_id
            FROM    ods_business_transactions s
            WHERE   s.doc_portfolio_type = c_portfolio_type_88
            AND     s.business_id = t.business_id
            AND     s.business_sequence >= t.business_sequence
            AND	    s.historical_link_flag = 'N'
            )
    ORDER BY t.business_id DESC;
    --
    -- Busca para DALMINE si existe 88
    CURSOR obtenerbusinessid_dalocho(pc_doc_comm_location_code        ods_business_transactions.doc_comm_location_code%TYPE,
                                     pc_document_year                 ods_business_transactions.document_year%TYPE,
                                     pc_doc_type_code                 ods_business_transactions.doc_type_code%TYPE,
                                     pc_document_number               ods_business_transactions.document_number%TYPE,
                                     pc_document_item_num             ods_business_transactions.document_item_num%TYPE)
    IS
    SELECT  business_id,
            business_sequence,
            doc_comm_location_code,
            secondary_costing_flag
    FROM    ods_business_transactions t
    WHERE   doc_comm_location_code = pc_doc_comm_location_code
    AND     document_year          = pc_document_year
    AND     (document_month = pc_doc_type_code OR TRIM(doc_type_code) = pc_doc_type_code)
    AND     document_number        = pc_document_number
    AND     document_item_num      = pc_document_item_num
    AND     business_reference_id  IS NULL
    AND     business_reference_seq IS NULL
    AND	    historical_link_flag = 'N'
    AND EXISTS
            (
            SELECT  business_id
            FROM    ods_business_transactions s
            WHERE   s.doc_portfolio_type = c_portfolio_type_88
            AND     s.business_id = t.business_id
            AND     s.business_sequence >= t.business_sequence
            AND	    s.historical_link_flag = 'N'
            )
    ORDER BY secondary_costing_flag ASC,
    	     business_id DESC;
    --
    -- Busca para el resto si existe 88
    CURSOR obtenerbusinessidocho(pc_doc_comm_location_code        ods_business_transactions.doc_comm_location_code%TYPE,
                                 pc_document_year                 ods_business_transactions.document_year%TYPE,
                                 pc_doc_type_code                 ods_business_transactions.doc_type_code%TYPE,
                                 pc_document_number               ods_business_transactions.document_number%TYPE,
                                 pc_document_item_num             ods_business_transactions.document_item_num%TYPE)
    IS
    SELECT  business_id,
            business_sequence,
            doc_comm_location_code,
            secondary_costing_flag
    FROM    ods_business_transactions t
    WHERE   doc_comm_location_code     = pc_doc_comm_location_code
    AND     document_year          = pc_document_year
    AND     TRIM(doc_type_code)    = pc_doc_type_code
    AND     document_number        = pc_document_number
    AND     document_item_num      = pc_document_item_num
    AND     business_reference_id  IS NULL
    AND     business_reference_seq IS NULL
    AND	    historical_link_flag = 'N'
    AND EXISTS
            (
            SELECT  business_id
            FROM    ods_business_transactions s
            WHERE   s.doc_portfolio_type = c_portfolio_type_88
            AND     s.business_id = t.business_id
            AND     s.business_sequence >= t.business_sequence
            AND	    s.historical_link_flag = 'N'
            )
    ORDER BY secondary_costing_flag ASC,
    	     business_id DESC;
    --
    -- busca para dalmine si no existe 88
    CURSOR obtenerbusinessid_dal(pc_doc_comm_location_code        ods_business_transactions.doc_comm_location_code%TYPE,
                                 pc_document_year                 ods_business_transactions.document_year%TYPE,
                                 pc_doc_type_code                 ods_business_transactions.doc_type_code%TYPE,
                                 pc_document_number               ods_business_transactions.document_number%TYPE,
                                 pc_document_item_num             ods_business_transactions.document_item_num%TYPE)
    IS
    SELECT  business_id,
            business_sequence,
            doc_comm_location_code,
            secondary_costing_flag
    FROM    ods_business_transactions t
    WHERE   doc_comm_location_code = pc_doc_comm_location_code
    AND     document_year          = pc_document_year
    AND     (document_month = pc_doc_type_code OR TRIM(doc_type_code) = pc_doc_type_code)
    AND     document_number        = pc_document_number
    AND     document_item_num      = pc_document_item_num
    AND     business_reference_id  IS NULL
    AND     business_reference_seq IS NULL
    AND	    historical_link_flag = 'N'
    ORDER BY secondary_costing_flag ASC,
    	     business_id DESC;

    -- Busca para el resto si no existe 88
    CURSOR obtenerbusinessid(pc_doc_comm_location_code        ods_business_transactions.doc_comm_location_code%TYPE,
                             pc_document_year                 ods_business_transactions.document_year%TYPE,
                             pc_doc_type_code                 ods_business_transactions.doc_type_code%TYPE,
                             pc_document_number               ods_business_transactions.document_number%TYPE,
                             pc_document_item_num             ods_business_transactions.document_item_num%TYPE)
    IS
    SELECT  business_id,
            business_sequence,
            doc_comm_location_code,
            secondary_costing_flag
    FROM    ods_business_transactions
    WHERE   doc_comm_location_code = pc_doc_comm_location_code
    AND     document_year          = pc_document_year
    AND     TRIM(doc_type_code)    = pc_doc_type_code
    AND     document_number        = pc_document_number
    AND     document_item_num      = pc_document_item_num
    AND     business_reference_id  IS NULL
    AND     business_reference_seq IS NULL
    AND	    historical_link_flag = 'N'
    ORDER BY secondary_costing_flag ASC,
    	     business_id DESC;
    --
    reg_bus_ods                     gbu_ft_facturacion%ROWTYPE;
    v_cantidad                      PLS_INTEGER;
    --
    v_doc_comm_location_code_new    ods_business_transactions.doc_comm_location_code%TYPE;
    v_doc_comm_location_code        ods_business_transactions.doc_comm_location_code%TYPE;
    v_doc_comm_location             ods_business_transactions.doc_comm_location_code%TYPE;
    v_document_year                 ods_business_transactions.document_year%TYPE;
    v_doc_type_code                 ods_business_transactions.doc_type_code%TYPE;
    v_document_number               ods_business_transactions.document_number%TYPE;
    v_document_item_num             ods_business_transactions.document_item_num%TYPE;
    v_business_id                   ods_business_transactions.business_id%TYPE;
    v_business_sequence             ods_business_transactions.business_sequence%TYPE;
    v_secondary_costing_flag	    ods_business_transactions.secondary_costing_flag%TYPE;
    v_po_ant                        tr_po;

BEGIN
    reg_Bus_ODS.c_Planta_Id         := 'UND';
    reg_Bus_ODS.c_Mill_Oi_Id        := 'UND-00000000000000-00000';
    reg_Bus_ODS.c_Po_Id             := 'UND-00000000000000-00000';
    reg_Bus_ODS.c_Branch_Oi_Ant_Id  := 'UND-00000000000000-00000';

    --Descompone la variable en todos sus subcampos
    v_doc_comm_location_code     := NVL(UPPER(SUBSTR(p_c_branch_oi_id,1,3)),'UND');
    v_doc_comm_location_code_new := NVL(UPPER(SUBSTR(p_c_branch_oi_id,1,3)),'UND');

    IF INSTR(TRANSLATE(UPPER(SUBSTR(p_c_branch_oi_id,5,4)),'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ -', '9999999999XXXXXXXXXXXXXXXXXXXXXXXXXXXX'),'X',1,1) > 0 THEN
      v_document_year := '0000';
    ELSE
      v_document_year := NVL(UPPER(SUBSTR(p_c_branch_oi_id,5,4)),'0000');
    END IF;

    v_doc_type_code := NVL(TRIM(SUBSTR(p_c_branch_oi_id,9,2)),'00');

    IF INSTR(TRANSLATE(UPPER(SUBSTR(p_c_branch_oi_id,11,8)),'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ -','9999999999XXXXXXXXXXXXXXXXXXXXXXXXXXXX'),'X',1,1) > 0 THEN
      v_document_number := '0';
    ELSE
      v_document_number := NVL(SUBSTR(p_c_branch_oi_id,11,8),'00000000');
    END IF;

    IF INSTR(TRANSLATE(UPPER(SUBSTR(p_c_branch_oi_id,20,5)),'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ -','9999999999XXXXXXXXXXXXXXXXXXXXXXXXXXXX'),'X',1,1) > 0 THEN
      v_document_item_num := 0;
    ELSE
      v_document_item_num := TO_NUMBER(NVL(SUBSTR(p_c_branch_oi_id,20,5),'00000'));
    END IF;

    -- Convierte de Codigo MILL a Codigo TEN
    IF v_doc_comm_location_code = 'SID' THEN
       v_doc_comm_location_code_new := 'G01';
    END IF;

    IF v_doc_comm_location_code = 'SIL' THEN
       v_doc_comm_location_code_new := 'R10';
    END IF;

    IF v_doc_comm_location_code = 'DAL' THEN
       v_doc_comm_location_code_new := 'H10';
    END IF;

    v_cantidad := 0;
    --
    -- Busca si existe un Codigo 88 en la cadena
    OPEN  buscacadenaochoocho(
                              v_doc_comm_location_code_new,
                              v_document_year,
                              v_doc_type_code,
                              v_document_number,
                              v_document_item_num);
    FETCH buscacadenaochoocho INTO v_cantidad;
    CLOSE buscacadenaochoocho;
    --
    IF v_cantidad >= 1 THEN
        -- Si existe en la cadena algun 88
        -- Si es dalmine busca por el mes sino por el tipo de documento
        IF v_doc_comm_location_code = 'DAL' THEN
            --
            OPEN  obtenerbusinessid_dalocho(
                                            v_doc_comm_location_code_new,
                                            v_document_year,
                                            v_doc_type_code,
                                            v_document_number,
                                            v_document_item_num);
            FETCH obtenerbusinessid_dalocho INTO v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag;
                  --
                  IF v_business_id IS NOT NULL AND v_business_sequence IS NOT NULL THEN
                     --
                     reg_bus_ods.c_mill_oi_id       := mill_valor_fn(v_business_id, v_business_sequence, v_secondary_costing_flag);
                     --se modifica para obtener la branc_oi_ant tanto para Fact como para Stock
                     reg_bus_ods.c_branch_oi_ant_id := so_anterior_fn(v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag);
                     --
                     IF p_origen = 'F' THEN
                           reg_bus_ods.c_po_id := po_asociada_fn(v_business_id, v_business_sequence, v_secondary_costing_flag);
                           v_po_ant            := po_anterior_fn(v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag);
                     END IF;
                     --
                  END IF;
                  --
            CLOSE obtenerbusinessid_dalocho;
            --
        ELSE
            --
            OPEN  obtenerbusinessidocho(
                                        v_doc_comm_location_code_new,
                                        v_document_year,
                                        v_doc_type_code,
                                        v_document_number,
                                        v_document_item_num);
            FETCH obtenerbusinessidocho INTO v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag;
                  --
                  IF v_business_id IS NOT NULL AND v_business_sequence IS NOT NULL THEN
                     --
                     reg_bus_ods.c_mill_oi_id       := mill_valor_fn(v_business_id, v_business_sequence, v_secondary_costing_flag);
                     --se modifica para obtener la branc_oi_ant tanto para Fact como para Stock
                     reg_bus_ods.c_branch_oi_ant_id := so_anterior_fn(v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag);
                     --
                     IF p_origen = 'F' THEN
                           reg_bus_ods.c_po_id := po_asociada_fn(v_business_id, v_business_sequence, v_secondary_costing_flag);
                           v_po_ant            := po_anterior_fn(v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag);
                     END IF;
                     --
                  END IF;
                  --
            CLOSE obtenerbusinessidocho;
            --
        END IF;
        --
    ELSE
        -- No existe en la cadena algun 88
        -- Si es dalmine busca por el mes sino por el tipo de documento
        IF v_doc_comm_location_code = 'DAL' THEN
            --
            OPEN  obtenerbusinessid_dal(
                                        v_doc_comm_location_code_new,
                                        v_document_year,
                                        v_doc_type_code,
                                        v_document_number,
                                        v_document_item_num);
            FETCH obtenerbusinessid_dal INTO v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag;
                  --
                  IF v_business_id IS NOT NULL AND v_business_sequence IS NOT NULL THEN
                     --
                     reg_Bus_ODS.c_Mill_Oi_Id       := mill_valor_fn(v_business_id, v_business_sequence, v_secondary_costing_flag);
                     --se modifica para obtener la branc_oi_ant tanto para Fact como para Stock
                     reg_Bus_ODS.c_Branch_Oi_Ant_Id := so_anterior_fn(v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag);
                     --
                     IF p_origen = 'F' THEN
                           reg_Bus_ODS.c_Po_Id := po_asociada_fn(v_business_id, v_business_sequence, v_secondary_costing_flag);
                           v_po_ant            := po_anterior_fn(v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag);
                     END IF;
                     --
                  END IF;
                  --
            CLOSE ObtenerBusinessId_DAL;
            --
        ELSE
            --
            OPEN  ObtenerBusinessId(
                                    v_doc_comm_location_code_new,
                                    v_document_year,
                                    v_doc_type_code,
                                    v_document_number,
                                    v_document_item_num);
            FETCH ObtenerBusinessId INTO v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag;
                  --
                  IF v_business_id IS NOT NULL AND v_business_sequence IS NOT NULL THEN
                     --
                     reg_Bus_ODS.c_Mill_Oi_Id       := mill_valor_fn(v_business_id, v_business_sequence, v_secondary_costing_flag);
                     --se modifica para obtener la branc_oi_ant tanto para Fact como para Stock
                     reg_Bus_ODS.c_Branch_Oi_Ant_Id := so_anterior_fn(v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag);
                     --
                     IF p_origen = 'F' THEN
                           reg_Bus_ODS.c_Po_Id := po_asociada_fn(v_business_id, v_business_sequence, v_secondary_costing_flag);
                           v_po_ant            := po_anterior_fn(v_business_id, v_business_sequence, v_doc_comm_location, v_secondary_costing_flag);
                     END IF;
                     --
                  END IF;
                  --
            CLOSE ObtenerBusinessId;
            --
        END IF;
        --
    END IF;
    --
    reg_bus_ods.selling_po_comm_loc_code := v_po_ant.selling_po_comm_loc_code;
    reg_bus_ods.selling_po_year          := v_po_ant.selling_po_year;
    reg_bus_ods.selling_po_month         := v_po_ant.selling_po_month;
    reg_bus_ods.selling_po_doc_type_code := v_po_ant.selling_po_doc_type_code;
    reg_bus_ods.selling_po_number        := v_po_ant.selling_po_number;
    reg_bus_ods.selling_po_itm_num       := v_po_ant.selling_po_itm_num;
    --
    reg_bus_ods.business_id_ref                := v_business_id;
    --
    RETURN (reg_bus_ods);
    --
END calculos_old_fn;
----------------------------------------------------------------------------------------
--////////////////// Nuevos procesos para obtencion de /////////////////////////////////
--//////////////////Conferma, So anteriores, Po asociada y anterior///////////////////
--//////////////////Basados en la ODS_BUSN_TRANSACTIONS_PEP_MV//////////////////////////
----------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   GET_BUSINESS_ID_SP
        Sistema: GBU Calculo Utilities
        Objetivo: Pasando por parámetro el PEP num reference busca en la ODS_BUSN_TRANSACTIONS_PEP_MV
        el PEP que no esté referenciado para obtener el BID, la secuencia el tipo de cadena (TUBULAR, ACCESORIO, SERVICIO)
        y la sociedad del PEP.
        Si encuentra más de un DIB, define por el que tenga informado un 88 para el tipo de cadena,
        si todos tienen un 88 define por el que tenga informado Ordenes de venta,
        Si todos tienen informado órdenes de venta define por el que tenga mayor cantidad de ramas de distintos tipos
        es decir, se evalúa si el BID tiene ramas de T A o S (teniendo mayor peso T luego A y por ultimo S)
        Por ultimo si todos tienen informado lo mismo define por el BID más Alto.

        Parámetros de entrada / Salida:
                                       IN(P_PEP_NUM_REFERENCE)
                                       OUT(P_FAMILY_TYPE_REF ,P_BUSINESS_ID ,P_BUSINESS_SEQUENCE, p_DOC_COMM_LOCATION_CODE)

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        05/04/2016      Romero Alejandro -      Creacion del procedure
*********************************************************************************************/
PROCEDURE get_business_id_sp(
          P_PEP_NUM_REFERENCE      IN     ODS_BUSN_TRANSACTIONS_PEP_MV.PEP_NUM_REFERENCE%TYPE
         ,P_FAMILY_TYPE_REF        IN OUT ODS_BUSN_TRANSACTIONS_PEP_MV.FAMILY_TYPE_REF%TYPE
         ,P_BUSINESS_ID            IN OUT ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_ID%TYPE
         ,P_BUSINESS_SEQUENCE      IN OUT ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_SEQUENCE%TYPE
         ,P_DOC_COMM_LOCATION_CODE IN OUT ODS_BUSN_TRANSACTIONS_PEP_MV.DOC_COMM_LOCATION_CODE%TYPE)
IS
BEGIN
     BEGIN
             SELECT
                    FINAL.FAMILY_TYPE_REF
                   ,FINAL.BUSINESS_ID
			             ,FINAL.BUSINESS_SEQUENCE
                   ,FINAL.DOC_COMM_LOCATION_CODE
             INTO   P_FAMILY_TYPE_REF
                   ,P_BUSINESS_ID
                   ,P_BUSINESS_SEQUENCE
                   ,P_DOC_COMM_LOCATION_CODE
             FROM(
                    SELECT
                           BT.FAMILY_TYPE_REF
                          ,BT.BUSINESS_ID
			                    ,BT.BUSINESS_SEQUENCE
                          ,BT.DOC_COMM_LOCATION_CODE
                          ,NVL((
                                SELECT MAX(1)
                                FROM
                                       ODS_BUSN_TRANSACTIONS_PEP_MV BT1
                                WHERE  1=1
                                AND    BT1.BUSINESS_ID             = BT.BUSINESS_ID
                                AND    BT1.BUSINESS_SEQUENCE       > BT.BUSINESS_SEQUENCE
                                AND    BT1.FAMILY_TYPE_REF         = BT.FAMILY_TYPE_REF
                                AND    BT1.DOC_PORTFOLIO_TYPE      = '88'
                                AND    BT1.MAIN_BOUGH_BY_TYPE_FLAG = 'Y'
                           ),0) AS WITH_88 -- Si la rama del tipo de familia evaluado tiene documentos de producción (88)
                          ,NVL((
                                SELECT MAX(1)
                                FROM
                                       ODS_BUSN_TRANSACTIONS_PEP_MV BT1
                                WHERE 1=1
                                AND   BT1.BUSINESS_ID              = BT.BUSINESS_ID
                                AND   BT1.BUSINESS_SEQUENCE        > BT.BUSINESS_SEQUENCE
                                AND   BT1.FAMILY_TYPE_REF          = BT.FAMILY_TYPE_REF
                                AND   BT1.DOC_PORTFOLIO_TYPE       = '02'
                                AND   BT1.MAIN_BOUGH_BY_TYPE_FLAG  = 'Y'
                           ),0) AS WITH_SO --Si la rama del tipo de familia evaluado tiene documentos de venta (02)
                          ,NVL((
                                SELECT SUM(DECODE(X.FAMILY_TYPE_REF,'TUBES','4','ACCESORIES','2','SERVICES','1','0'))
                                FROM(
                                      SELECT
                                             BT1.FAMILY_TYPE_REF
				                              FROM   ODS_BUSN_TRANSACTIONS_PEP_MV BT1
                                      WHERE  1=1
                                      AND    BT1.BUSINESS_ID             = BT.BUSINESS_ID
                                      AND    BT1.BUSINESS_SEQUENCE       > BT.BUSINESS_SEQUENCE
                                      AND    BT1.DOC_PORTFOLIO_TYPE      IN('88','02')
                                      AND    BT1.MAIN_BOUGH_BY_TYPE_FLAG = 'Y'
                                      GROUP BY
                                             BT1.FAMILY_TYPE_REF
					                          )X
                          ),0)AS PESO--Se evalúa si el BID tiene ramas de T A o S (teniendo mayor peso T luego A y por ultimo S)
                    FROM  ODS_BUSN_TRANSACTIONS_PEP_MV BT
                    WHERE 1=1
                    AND   BT.BUSINESS_REFERENCE_ID IS NULL
                    AND   BT.PEP_NUM_REFERENCE = P_PEP_NUM_REFERENCE
                    ORDER BY  WITH_88     DESC
                             ,WITH_SO     DESC
                             ,PESO        DESC
                             ,BUSINESS_ID DESC
                             ,BUSINESS_SEQUENCE ASC
             )FINAL
             WHERE 1=1
             AND   ROWNUM =1;

     EXCEPTION
     WHEN NO_DATA_FOUND THEN
                        P_FAMILY_TYPE_REF   :='';
                        P_BUSINESS_ID       :=-1;
			                  P_BUSINESS_SEQUENCE :=-1;

     WHEN OTHERS THEN
                        P_FAMILY_TYPE_REF   :='';
                        P_BUSINESS_ID       :=-1;
			                  P_BUSINESS_SEQUENCE :=-1;
     END;
END get_business_id_sp;
----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   GET_PRINCIPAL_CHAIN_SP
        Sistema: GBU Calculo Utilities
        Objetivo: Pasando por parámetro el PEP num reference busca en la ODS_BUSN_TRANSACTIONS_PEP_MV
        el PEP que no esté referenciado para obtener el BID, la secuencia el tipo de cadena (TUBULAR, ACCESORIO, SERVICIO)
        y la sociedad del PEP.
        Si encuentra más de un DIB, define por el que tenga informado un 88 para el tipo de cadena,
        si todos tienen un 88 define por el que tenga informado Ordenes de venta,
        Si todos tienen informado órdenes de venta define por el que tenga mayor cantidad de ramas de distintos tipos
        es decir, se evalúa si el BID tiene ramas de T A o S (teniendo mayor peso T luego A y por ultimo S)
        Por ultimo si todos tienen informado lo mismo define por el BID más Alto.

        Parámetros de entrada / Salida:
                                       IN(P_PEP_NUM_REFERENCE)
                                       OUT(P_FAMILY_TYPE_REF ,P_BUSINESS_ID ,P_BUSINESS_SEQUENCE, p_DOC_COMM_LOCATION_CODE)

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        05/04/2016      Romero Alejandro -      Creación del procedure
*********************************************************************************************/
PROCEDURE get_principal_chain_sp (
          P_BUSN_TRX_PEP_TABLE     IN OUT NOCOPY ODS_BUSN_TRX_PEP_TABLE_TYPE
         ,P_BUSINESS_ID            IN ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_ID%TYPE
         ,P_BUSINESS_SEQUENCE      IN ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_SEQUENCE%TYPE
         ,P_FAMILY_TYPE_REF        IN ODS_BUSN_TRANSACTIONS_PEP_MV.FAMILY_TYPE_REF%TYPE
         ,P_DOC_COMM_LOCATION_CODE IN ODS_BUSN_TRANSACTIONS_PEP_MV.DOC_COMM_LOCATION_CODE%TYPE)
IS
        /*Cursor para obtener la PO asociada.
        La primer orden de compra anterior (< secuencia) al documento evaluado.
        Si el DOC Type Code es un IM y las sociedades son TAM,ALG,TAV o G01, obtiene el Documento Anterior
        (Secuencia -1) y que sea un 88.
        Por eso el cursor levanta todos los 01 y 88 consecuencia menor al pep evaluado y los ordena para
        recorrerlos de mayor a menor*/
        CURSOR cur_chain_to_po_associated
        IS
        SELECT
               BT.FAMILY_TYPE_REF
              ,BT.MAIN_BOUGH_BY_TYPE_FLAG
              ,BT.PEP_NUM_REFERENCE
              ,BT.BUSINESS_ID
              ,BT.BUSINESS_SEQUENCE
              ,BT.DOC_PORTFOLIO_TYPE
              ,BT.DOC_COMM_LOCATION_CODE
              ,BT.DOCUMENT_YEAR
              ,BT.DOCUMENT_MONTH
              ,RPAD(BT.DOC_TYPE_CODE, 2, ' ') AS DOC_TYPE_CODE
              ,BT.DOCUMENT_NUMBER
              ,BT.DOCUMENT_ITEM_NUM
              ,BT.DOCUMENT_SPLIT_NUM
        FROM
               ODS_BUSN_TRANSACTIONS_PEP_MV BT
        WHERE  1=1
        AND    BT.BUSINESS_ID             = P_BUSINESS_ID
        AND    BT.BUSINESS_SEQUENCE       < P_BUSINESS_SEQUENCE
        AND    BT.FAMILY_TYPE_REF         = P_FAMILY_TYPE_REF
        AND    BT.DOC_PORTFOLIO_TYPE      IN('01','88')
        ORDER BY
               BT.BUSINESS_SEQUENCE DESC;

        /*Cursor para Obtener las órdenes de venta y compras posteriores (secuencia >) al pep evaluado
        Para el caso de las órdenes de venta no tienen que ser de la misma sociedad de la anterior.
        y la orden de compra se obtiene solo una (la primera que tenga la misma sociedad del Pep evaluado)
        Cuando se encuentra un 88 se deja de levantar documentos.
        No se trae el documento evaluado solo que el mismo sea un 88 (para devolverlo como conferma)*/
        CURSOR cur_principal_chain
        IS
        SELECT
               BT.FAMILY_TYPE_REF
              ,BT.MAIN_BOUGH_BY_TYPE_FLAG
              ,BT.PEP_NUM_REFERENCE
              ,BT.BUSINESS_ID
              ,BT.BUSINESS_SEQUENCE
              ,BT.DOC_PORTFOLIO_TYPE
              ,BT.DOC_COMM_LOCATION_CODE
              ,BT.DOCUMENT_YEAR
              ,BT.DOCUMENT_MONTH
              ,RPAD(BT.DOC_TYPE_CODE, 2, ' ') AS DOC_TYPE_CODE
              ,BT.DOCUMENT_NUMBER
              ,BT.DOCUMENT_ITEM_NUM
              ,BT.DOCUMENT_SPLIT_NUM
        FROM
               ODS_BUSN_TRANSACTIONS_PEP_MV BT
        WHERE  1=1
        AND    BT.BUSINESS_ID             = P_BUSINESS_ID
        AND    BT.BUSINESS_SEQUENCE      >= P_BUSINESS_SEQUENCE
        AND    BT.FAMILY_TYPE_REF         = P_FAMILY_TYPE_REF
        AND    BT.DOC_PORTFOLIO_TYPE      IN('01','02','88')
        AND    BT.MAIN_BOUGH_BY_TYPE_FLAG = 'Y'
        ORDER BY
               BT.BUSINESS_SEQUENCE ASC;

        v_chain_to_po_associated_rec   cur_chain_to_po_associated%ROWTYPE;
        v_principal_chain_rec          cur_principal_chain%ROWTYPE;
        v_found_01                     BOOLEAN;
        v_insert                       BOOLEAN;
        v_sequence                     PLS_INTEGER;
        V_DOC_COMM_LOCATION_CODE       ODS_BUSN_TRANSACTIONS_PEP_MV.DOC_COMM_LOCATION_CODE%TYPE;
BEGIN
     v_insert                := FALSE;
     v_found_01              := FALSE;
     v_sequence              := 0;
     BEGIN
        --Recorro Cursos1
        FOR v_chain_to_po_associated_rec IN cur_chain_to_po_associated
        LOOP
             /*Si es una orden de compra, el doc type es un IM y la sociedad es
             TAM, ALG, TAV o G01, marco variable para indicar que encontró un 01 y guarda la secuencia (-1)
             en donde tendría que estar el 88.
             Si no es IM o las sociedades no son las indicadas, devuelve el 01 y sale del cursor*/
             IF v_chain_to_po_associated_rec.DOC_PORTFOLIO_TYPE  = '01' THEN
                   IF v_chain_to_po_associated_rec.DOC_TYPE_CODE = 'IM'
                      AND
                      v_chain_to_po_associated_rec.DOC_COMM_LOCATION_CODE IN ('TAM','ALG','TAV','G01')
                   THEN
                           v_found_01:= TRUE;
                           v_sequence:= v_chain_to_po_associated_rec.BUSINESS_SEQUENCE -1;
                   ELSE
                           v_insert  := TRUE;
                   END IF;
             ELSE --Es un DOC_PORTFOLIO_TYPE  = '88'
                    /*Si es un 88 y ya encontró un 01 previo y la secuencia coincide
                    con la guardad, Inserto y sale del cursor */
                    IF v_found_01
                       AND
                       v_sequence = v_chain_to_po_associated_rec.BUSINESS_SEQUENCE
                    THEN
                       v_insert  := TRUE;
                    END IF;
             END IF;

             IF v_insert THEN
                P_BUSN_TRX_PEP_TABLE.EXTEND;
                P_BUSN_TRX_PEP_TABLE(P_BUSN_TRX_PEP_TABLE.COUNT) :=
                ODS_BUSN_TRX_PEP_ROW_TYPE(
                                 v_chain_to_po_associated_rec.FAMILY_TYPE_REF
                                ,v_chain_to_po_associated_rec.PEP_NUM_REFERENCE
                                ,v_chain_to_po_associated_rec.BUSINESS_ID
                                ,v_chain_to_po_associated_rec.BUSINESS_SEQUENCE
                                ,'PO'--DOC_PORTFOLIO_TYPE como puede ser 01 o 88 para poder acceder luego la guardo como PO
                                ,v_chain_to_po_associated_rec.DOC_COMM_LOCATION_CODE
                                ,v_chain_to_po_associated_rec.DOCUMENT_YEAR
                                ,v_chain_to_po_associated_rec.DOCUMENT_MONTH
                                ,v_chain_to_po_associated_rec.DOC_TYPE_CODE
                                ,v_chain_to_po_associated_rec.DOCUMENT_NUMBER
                                ,v_chain_to_po_associated_rec.DOCUMENT_ITEM_NUM
                                ,v_chain_to_po_associated_rec.DOCUMENT_SPLIT_NUM
                                );
                EXIT;
             END IF;
        END LOOP;

        v_found_01              := FALSE;
        V_DOC_COMM_LOCATION_CODE:= P_DOC_COMM_LOCATION_CODE;

        FOR v_principal_chain_rec IN cur_principal_chain
        LOOP --Si no es el PEP evaluado:
             IF v_principal_chain_rec.BUSINESS_SEQUENCE > P_BUSINESS_SEQUENCE THEN
                  IF v_principal_chain_rec.DOC_PORTFOLIO_TYPE = '02' THEN -- es un 02
                     --La sociedad es distinta a la guardada(la primera vez es la del pep evaluado) no inserto
                     IF v_principal_chain_rec.DOC_COMM_LOCATION_CODE = V_DOC_COMM_LOCATION_CODE THEN
                        v_insert:= FALSE;
                     ELSE--Si la sociedad es distinta Inserto y guardo cual es la sociedad.
                        v_insert:= TRUE;
                        V_DOC_COMM_LOCATION_CODE:= v_principal_chain_rec.DOC_COMM_LOCATION_CODE;
                     END IF;
                  ELSIF v_principal_chain_rec.DOC_PORTFOLIO_TYPE = '01' THEN --Es un 01
                      --Como solo inserto un 01 pregunto si la encontró un 01 sino verifico que la sociedad sea la misma, e inserto
                      IF (NOT v_found_01) AND  (v_principal_chain_rec.DOC_COMM_LOCATION_CODE = P_DOC_COMM_LOCATION_CODE) THEN
                            v_insert  := TRUE;
                            v_found_01:= TRUE;
                       ELSE
                            v_insert:= FALSE;
                       END IF;
                  ELSE--Es un 88
                     v_insert:= TRUE;
                  END IF;
             ELSE--Si es el PEP evaluado verifico que sea un 88 solo en ese caso inserto.
                 IF   v_principal_chain_rec.DOC_PORTFOLIO_TYPE = '88' THEN
                      v_insert:= TRUE;
                 ELSE
                      v_insert:= FALSE;
                 END IF;
             END IF;


             IF v_insert THEN
                  P_BUSN_TRX_PEP_TABLE.EXTEND;
                  P_BUSN_TRX_PEP_TABLE(P_BUSN_TRX_PEP_TABLE.COUNT) :=
                  ODS_BUSN_TRX_PEP_ROW_TYPE(
                                    v_principal_chain_rec.FAMILY_TYPE_REF
                                   ,v_principal_chain_rec.PEP_NUM_REFERENCE
                                   ,v_principal_chain_rec.BUSINESS_ID
                                   ,v_principal_chain_rec.BUSINESS_SEQUENCE
                                   ,v_principal_chain_rec.DOC_PORTFOLIO_TYPE
                                   ,v_principal_chain_rec.DOC_COMM_LOCATION_CODE
                                   ,v_principal_chain_rec.DOCUMENT_YEAR
                                   ,v_principal_chain_rec.DOCUMENT_MONTH
                                   ,v_principal_chain_rec.DOC_TYPE_CODE
                                   ,v_principal_chain_rec.DOCUMENT_NUMBER
                                   ,v_principal_chain_rec.DOCUMENT_ITEM_NUM
                                   ,v_principal_chain_rec.DOCUMENT_SPLIT_NUM
                                   );
                  IF v_principal_chain_rec.DOC_PORTFOLIO_TYPE = '88' THEN--Si encontró un 88 sale del cursor.
                    EXIT;
                  END IF;
             END IF;
        END LOOP;

        IF P_BUSN_TRX_PEP_TABLE.COUNT = 0 THEN
           RAISE NO_DATA_FOUND;
        END IF;

     EXCEPTION
              WHEN NO_DATA_FOUND THEN
              P_BUSN_TRX_PEP_TABLE.DELETE;

              WHEN OTHERS THEN
              P_BUSN_TRX_PEP_TABLE.DELETE;
     END;
END get_principal_chain_sp;
----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   SO_ANTERIORES_NEW_FN
        Sistema: GBU Calculo Utilities
        Objetivo: Pasado por parámetro la cadena principal, toma todos los documentos 02
        Y 88 si es que se solicitó en el parámetro opcional P_WITH_88

        Parámetros de entrada / Salida:
                                       IN(P_BUSN_TRX_PEP_TABLE)
                                       gbu_cadena_table_type

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        05/04/2016      Romero Alejandro -      Creacion del procedure
*********************************************************************************************/
FUNCTION so_anteriores_new_fn(
         P_BUSN_TRX_PEP_TABLE     IN OUT NOCOPY ODS_BUSN_TRX_PEP_TABLE_TYPE
        ,P_WITH_88                IN ODS_COMMON_PKG.S_STR_BOOLEAN DEFAULT ODS_COMMON_PKG.C_STR_FALSE)
RETURN GBU_CADENA_TABLE_TYPE
IS
     V_CADENA_OBJ        GBU_CADENA_TABLE_TYPE:= GBU_CADENA_TABLE_TYPE();
BEGIN
     BEGIN
	        SELECT GBU_CADENA_ROW_TYPE(
                 BT.PEP_NUM_REFERENCE) BULK COLLECT INTO V_CADENA_OBJ
     	    FROM   TABLE(CAST(P_BUSN_TRX_PEP_TABLE AS ODS_BUSN_TRX_PEP_TABLE_TYPE)) BT
          WHERE  1=1
          AND    BT.DOC_PORTFOLIO_TYPE = '02'
          OR
             (
                 BT.DOC_PORTFOLIO_TYPE = '88'
                 AND
                 P_WITH_88             = ODS_COMMON_PKG.C_STR_TRUE
              )
          ORDER BY
                 BT.BUSINESS_SEQUENCE ASC;

          IF V_CADENA_OBJ.COUNT = 0 THEN
             RAISE NO_DATA_FOUND;
          END IF;

     EXCEPTION
          WHEN NO_DATA_FOUND THEN
          SELECT GBU_CADENA_ROW_TYPE('UND-00000000000000-00000') BULK COLLECT INTO V_CADENA_OBJ FROM DUAL;

          WHEN OTHERS THEN
          SELECT GBU_CADENA_ROW_TYPE('UND-00000000000000-00000') BULK COLLECT INTO V_CADENA_OBJ FROM DUAL;
     END;
RETURN  V_CADENA_OBJ;
END so_anteriores_new_fn;
----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   SO_ANTERIORES_NEW_FN - (SOBRECARGA)
        Sistema: GBU Calculo Utilities
        Objetivo: Pasado por parámetro el PEP, (y opcionalmente si se quiere obtener el documento 88)
        y también opcionalmente el tipo de rama que se quiere obtener (TUBULAR-ACCESORIO-SERVICIO)
        Obtiene el Bid correspondiente al PEP por medio de GET_BUSINESS_ID_SP.
        Luego se reconstruye la rama por medio de GET_PRINCIPAL_CHAIN_SP (si se solicitó
        un tipo determinado de cadena se pasa este en lugar del correspondiente al PEP principal)
        Por ultimo devuelve todas las SO por medio de so_anteriores_new_fn

        Parámetros de entrada / Salida:
                                       IN(P_PEP_NUM_REFERENCE)[Opcional P_FAMILY_TYPE_REF, P_WITH_88]
                                       OUT(gbu_cadena_table_type)

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        05/04/2016      Romero Alejandro -      Creación del procedure
*********************************************************************************************/
FUNCTION so_anteriores_new_fn(
         P_C_BRANCH_OI_ID    IN GBU_FT_FACTURACION.C_BRANCH_OI_ID%TYPE
        ,P_FAMILY_TYPE_REF   IN ODS_BUSN_TRANSACTIONS_PEP_MV.FAMILY_TYPE_REF%TYPE  DEFAULT NULL
        ,P_WITH_88           IN ODS_COMMON_PKG.S_STR_BOOLEAN                       DEFAULT ODS_COMMON_PKG.C_STR_FALSE)
RETURN GBU_CADENA_TABLE_TYPE
IS
     V_FAMILY_TYPE_REF               ODS_BUSN_TRANSACTIONS_PEP_MV.FAMILY_TYPE_REF%TYPE;
     V_BUSINESS_ID                   ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_ID%TYPE;
     V_BUSINESS_SEQUENCE             ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_SEQUENCE%TYPE;
     V_DOC_COMM_LOCATION_CODE        ODS_BUSN_TRANSACTIONS_PEP_MV.DOC_COMM_LOCATION_CODE%TYPE;

     V_BUSN_TRX_PEP_FINAL_TABLE      ODS_BUSN_TRX_PEP_TABLE_TYPE:= ODS_BUSN_TRX_PEP_TABLE_TYPE();
     V_CADENA_OBJ                    GBU_CADENA_TABLE_TYPE      := GBU_CADENA_TABLE_TYPE();
BEGIN
      /*Obtengo BID*/
       GET_BUSINESS_ID_SP(
                       P_PEP_NUM_REFERENCE      => P_C_BRANCH_OI_ID
                      ,P_BUSINESS_ID            => V_BUSINESS_ID
                      ,P_BUSINESS_SEQUENCE      => V_BUSINESS_SEQUENCE
                      ,P_FAMILY_TYPE_REF        => V_FAMILY_TYPE_REF
                      ,P_DOC_COMM_LOCATION_CODE => V_DOC_COMM_LOCATION_CODE);

       IF P_FAMILY_TYPE_REF IS NOT NULL THEN
          V_FAMILY_TYPE_REF:=P_FAMILY_TYPE_REF;
       END IF;

      /*Obtengo Rama Principal*/
       GET_PRINCIPAL_CHAIN_SP(
                           P_BUSN_TRX_PEP_TABLE     => V_BUSN_TRX_PEP_FINAL_TABLE
                          ,P_BUSINESS_ID            => V_BUSINESS_ID
                          ,P_BUSINESS_SEQUENCE      => V_BUSINESS_SEQUENCE
                          ,P_FAMILY_TYPE_REF        => V_FAMILY_TYPE_REF
                          ,P_DOC_COMM_LOCATION_CODE => V_DOC_COMM_LOCATION_CODE);

      /*Retorno Sales Orders Anteriores*/
       V_CADENA_OBJ:= SO_ANTERIORES_NEW_FN(
                                        P_BUSN_TRX_PEP_TABLE => V_BUSN_TRX_PEP_FINAL_TABLE
                                       ,P_WITH_88            => P_WITH_88);

       RETURN  V_CADENA_OBJ;
END so_anteriores_new_fn;
/*********************************************************************************************
Nombre del programa:   SO_ANTERIORES_NEW_FN - (SOBRECARGA)
        Sistema: GBU Calculo Utilities
        Objetivo: Pasado por parámetro el Bid, Secuencia, Sociedad del pep principal, 
        Tipo de familia que se quiere obtener (TUBULAR-ACCESORIO-SERVICIO),(y opcionalmente si se quiere 
        obtener el documento 88)
        Reconstruye la rama por medio de GET_PRINCIPAL_CHAIN_SP 
        Por ultimo devuelve todas las SO por medio de so_anteriores_new_fn

        Parámetros de entrada / Salida:
                                       IN(BUSINESS_ID, BUSINESS_SEQUENCE, FAMILY_TYPE_REF, DOC_COMM_LOCATION_CODE)[Opcional P_WITH_88]
                                       OUT(gbu_cadena_table_type)

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        31/01/2017      Romero Alejandro -      Creación del procedure
*********************************************************************************************/
FUNCTION so_anteriores_new_fn(
          P_BUSINESS_ID            IN  ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_ID%TYPE
         ,P_BUSINESS_SEQUENCE      IN  ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_SEQUENCE%TYPE
         ,P_FAMILY_TYPE_REF        IN  ODS_BUSN_TRANSACTIONS_PEP_MV.FAMILY_TYPE_REF%TYPE		 
         ,P_DOC_COMM_LOCATION_CODE IN  ODS_BUSN_TRANSACTIONS_PEP_MV.DOC_COMM_LOCATION_CODE%TYPE
		     ,P_WITH_88           	   IN  ODS_COMMON_PKG.S_STR_BOOLEAN DEFAULT ODS_COMMON_PKG.C_STR_FALSE
		 )
RETURN GBU_CADENA_TABLE_TYPE
IS
     V_BUSN_TRX_PEP_FINAL_TABLE      ODS_BUSN_TRX_PEP_TABLE_TYPE:= ODS_BUSN_TRX_PEP_TABLE_TYPE();
     V_CADENA_OBJ                    GBU_CADENA_TABLE_TYPE      := GBU_CADENA_TABLE_TYPE();
BEGIN
      
      /*Obtengo Rama Principal*/
       GET_PRINCIPAL_CHAIN_SP(
                           P_BUSN_TRX_PEP_TABLE     => V_BUSN_TRX_PEP_FINAL_TABLE
                          ,P_BUSINESS_ID            => P_BUSINESS_ID
                          ,P_BUSINESS_SEQUENCE      => P_BUSINESS_SEQUENCE
                          ,P_FAMILY_TYPE_REF        => P_FAMILY_TYPE_REF
                          ,P_DOC_COMM_LOCATION_CODE => P_DOC_COMM_LOCATION_CODE);

      /*Retorno Sales Orders Anteriores*/
       V_CADENA_OBJ:= SO_ANTERIORES_NEW_FN(
                                        P_BUSN_TRX_PEP_TABLE => V_BUSN_TRX_PEP_FINAL_TABLE
                                       ,P_WITH_88            => P_WITH_88);

       RETURN  V_CADENA_OBJ;
END so_anteriores_new_fn;
----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   PO_ASOCIADA_NEW_FN
        Sistema: GBU Calculo Utilities
        Objetivo: Pasado por parámetro la cadena principal, toma el documento PO si existe

        Parámetros de entrada / Salida:
                                       IN(P_BUSN_TRX_PEP_TABLE)
                                       VARCHAR2

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        05/04/2016      Romero Alejandro -      Creación del procedure
*********************************************************************************************/
FUNCTION po_asociada_new_fn(
         P_BUSN_TRX_PEP_TABLE     IN OUT NOCOPY ODS_BUSN_TRX_PEP_TABLE_TYPE)
RETURN VARCHAR2
IS
    V_C_PO_ID                    GBU_FT_FACTURACION.C_PO_ID%TYPE;
BEGIN
     BEGIN
	         SELECT
                  BT.PEP_NUM_REFERENCE INTO V_C_PO_ID
           FROM   TABLE(CAST(P_BUSN_TRX_PEP_TABLE AS ODS_BUSN_TRX_PEP_TABLE_TYPE)) BT
           WHERE  1=1
           AND    BT.DOC_PORTFOLIO_TYPE ='PO';

     EXCEPTION
              WHEN NO_DATA_FOUND THEN
              V_C_PO_ID:= 'UND-00000000000000-00000';

              WHEN OTHERS THEN
              V_C_PO_ID:= 'UND-00000000000000-00000';
     END;
RETURN  V_C_PO_ID;
END po_asociada_new_fn;
------------------------------------------------------------------------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   PO_ANTERIOR_NEW_FN
        Sistema: GBU Calculo Utilities
        Objetivo: Pasado por parámetro la cadena principal, toma el documento 01 si existe

        Parámetros de entrada / Salida:
                                       IN(P_BUSN_TRX_PEP_TABLE)
                                       OUT (RECORD TYPE tr_po)

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        05/04/2016      Romero Alejandro -      Creación del procedure
*********************************************************************************************/
FUNCTION po_anterior_new_fn(
         P_BUSN_TRX_PEP_TABLE     IN OUT NOCOPY ODS_BUSN_TRX_PEP_TABLE_TYPE)
RETURN TR_PO
IS
       V_PO  TR_PO;
BEGIN
     BEGIN
           SELECT
                  BT.DOC_COMM_LOCATION_CODE
				         ,BT.DOCUMENT_YEAR
				         ,BT.DOCUMENT_MONTH
                 ,BT.DOC_TYPE_CODE
                 ,BT.DOCUMENT_NUMBER
                 ,BT.DOCUMENT_ITEM_NUM INTO V_PO
           FROM  TABLE(CAST(P_BUSN_TRX_PEP_TABLE AS ODS_BUSN_TRX_PEP_TABLE_TYPE)) BT
           WHERE  1=1
           AND    BT.DOC_PORTFOLIO_TYPE = '01';

     EXCEPTION
              WHEN NO_DATA_FOUND THEN
              V_PO:= NULL;

              WHEN OTHERS THEN
              V_PO:= NULL;
     END;
RETURN  V_PO;
END po_anterior_new_fn;
------------------------------------------------------------------------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   MILL_VALOR_NEW_FN
        Sistema: GBU Calculo Utilities
        Objetivo: Pasado por parámetro la cadena principal, toma el documento 88 si existe

        Parámetros de entrada / Salida:
                                       IN(P_BUSN_TRX_PEP_TABLE)
                                       OUT (VARCHAR2

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        05/04/2016      Romero Alejandro -      Creación del procedure
*********************************************************************************************/
FUNCTION mill_valor_new_fn(
         P_BUSN_TRX_PEP_TABLE     IN OUT NOCOPY ODS_BUSN_TRX_PEP_TABLE_TYPE)
RETURN VARCHAR2
IS
     V_C_MILL_OI_ID                GBU_FT_FACTURACION.C_MILL_OI_ID%TYPE;
BEGIN
     BEGIN
	         SELECT
                  BT.PEP_NUM_REFERENCE INTO V_C_MILL_OI_ID
           FROM   TABLE(CAST(P_BUSN_TRX_PEP_TABLE AS ODS_BUSN_TRX_PEP_TABLE_TYPE)) BT
           WHERE  1=1
           AND    BT.DOC_PORTFOLIO_TYPE ='88';

     EXCEPTION
              WHEN NO_DATA_FOUND THEN
              V_C_MILL_OI_ID:= 'UND-00000000000000-00000';

              WHEN OTHERS THEN
              V_C_MILL_OI_ID:= 'UND-00000000000000-00000';
     END;
RETURN  V_C_MILL_OI_ID;
END mill_valor_new_fn;
------------------------------------------------------------------------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   CALCULOS_NEW_FN
        Sistema: GBU Calculo Utilities
        Objetivo: Pasado por parámetro el PEP Obtiene el Bid correspondiente al PEP por medio de
        GET_BUSINESS_ID_SP. Luego se reconstruye la rama por medio de GET_PRINCIPAL_CHAIN_SP
        Por ultimo completa El valor de business_id_ref.
        El valor de c_mill_oi_id con mill_valor_new_fn
        El valor de c_branch_oi_ant_id con so_anteriores_new_fn
        Si el parámetro p_origen indica que se está evaluando un PEP de facturación (F) entonces
        El valor de c_po_id con po_asociada_new_fn
        Y por último los valores de selling_po con po_anterior_new_fn

        Parámetros de entrada / Salida:
                                       IN p_c_branch_oi_id, p_origen
                                       OUT (bu_ft_facturacion%ROWTYPE

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        05/04/2016      Romero Alejandro -      Creación del procedure
*********************************************************************************************/
FUNCTION calculos_new_fn(
         P_C_BRANCH_OI_ID    IN GBU_FT_FACTURACION.C_BRANCH_OI_ID%TYPE
        ,P_ORIGEN            IN VARCHAR2)
RETURN GBU_FT_FACTURACION%ROWTYPE
IS
       V_FAMILY_TYPE_REF               ODS_BUSN_TRANSACTIONS_PEP_MV.FAMILY_TYPE_REF%TYPE;
       V_BUSINESS_ID                   ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_ID%TYPE;
       V_BUSINESS_SEQUENCE             ODS_BUSN_TRANSACTIONS_PEP_MV.BUSINESS_SEQUENCE%TYPE;
       V_DOC_COMM_LOCATION_CODE        ODS_BUSN_TRANSACTIONS_PEP_MV.DOC_COMM_LOCATION_CODE%TYPE;

       REG_BUS_ODS                     GBU_FT_FACTURACION%ROWTYPE;
       V_PO_SALIDA                     TR_PO;
       V_BUSN_TRX_PEP_FINAL_TABLE      ODS_BUSN_TRX_PEP_TABLE_TYPE:= ODS_BUSN_TRX_PEP_TABLE_TYPE();
BEGIN
      /*Obtengo BID*/
       GET_BUSINESS_ID_SP(
                           P_PEP_NUM_REFERENCE      => P_C_BRANCH_OI_ID
                          ,P_BUSINESS_ID            => V_BUSINESS_ID
                          ,P_BUSINESS_SEQUENCE      => V_BUSINESS_SEQUENCE
                          ,P_FAMILY_TYPE_REF        => V_FAMILY_TYPE_REF
                          ,P_DOC_COMM_LOCATION_CODE => V_DOC_COMM_LOCATION_CODE);

      /*Obtengo Rama Principal*/
       GET_PRINCIPAL_CHAIN_SP(
                           P_BUSN_TRX_PEP_TABLE     => V_BUSN_TRX_PEP_FINAL_TABLE
                          ,P_BUSINESS_ID            => V_BUSINESS_ID
                          ,P_BUSINESS_SEQUENCE      => V_BUSINESS_SEQUENCE
                          ,P_FAMILY_TYPE_REF        => V_FAMILY_TYPE_REF
                          ,P_DOC_COMM_LOCATION_CODE => V_DOC_COMM_LOCATION_CODE);

      /*Retorno BID*/
       REG_BUS_ODS.BUSINESS_ID_REF    := V_BUSINESS_ID;

      /* Retorno Conferma*/
       REG_BUS_ODS.C_MILL_OI_ID       := MILL_VALOR_NEW_FN(
                                         P_BUSN_TRX_PEP_TABLE => V_BUSN_TRX_PEP_FINAL_TABLE);

      /* Retorno Sales Orders Anteriores y me quedo con la primer SO*/
       REG_BUS_ODS.C_BRANCH_OI_ANT_ID := SO_ANTERIORES_NEW_FN(
                                         P_BUSN_TRX_PEP_TABLE => V_BUSN_TRX_PEP_FINAL_TABLE)(1).OI_ID;

      /* Si se está calculando para Facturación*/
       IF P_ORIGEN = 'F' THEN
                  /*Retorno Purcharse Asociada*/
                  REG_BUS_ODS.C_PO_ID := PO_ASOCIADA_NEW_FN(
                                         P_BUSN_TRX_PEP_TABLE => V_BUSN_TRX_PEP_FINAL_TABLE);

                  /*Retorno Purcharse Anterior*/
                  V_PO_SALIDA         := PO_ANTERIOR_NEW_FN(
                                         P_BUSN_TRX_PEP_TABLE => V_BUSN_TRX_PEP_FINAL_TABLE);

                  REG_BUS_ODS.SELLING_PO_COMM_LOC_CODE := V_PO_SALIDA.SELLING_PO_COMM_LOC_CODE;
                  REG_BUS_ODS.SELLING_PO_YEAR          := V_PO_SALIDA.SELLING_PO_YEAR;
                  REG_BUS_ODS.SELLING_PO_MONTH         := V_PO_SALIDA.SELLING_PO_MONTH;
                  REG_BUS_ODS.SELLING_PO_DOC_TYPE_CODE := V_PO_SALIDA.SELLING_PO_DOC_TYPE_CODE;
                  REG_BUS_ODS.SELLING_PO_NUMBER        := V_PO_SALIDA.SELLING_PO_NUMBER;
                  REG_BUS_ODS.SELLING_PO_ITM_NUM       := V_PO_SALIDA.SELLING_PO_ITM_NUM;
       END IF;

       RETURN REG_BUS_ODS;
END calculos_new_fn;
/*********************************************************************************************
Nombre del programa:   CALCULOS_FN
        Sistema: GBU Calculo Utilities
        Objetivo: Esta función es llamada en varios lados.
        Por lo tanto para tener controlada la implementación se renombro esta función con calculos_OLD_fn
        Y dependiendo del valor de la tabla GBU_ERROR_TO_DEFINE DEF para en D_NAME ='CADENA_PEP_MV'
        llamara a calculos_OLD_fn o calculos_New_fn

        Parámetros de entrada / Salida:
                                       IN p_c_branch_oi_id, p_origen
                                       OUT (bu_ft_facturacion%ROWTYPE

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        05/04/2016      Romero Alejandro -      Creación del procedure
*********************************************************************************************/
FUNCTION calculos_fn(
         P_C_BRANCH_OI_ID    IN GBU_FT_FACTURACION.C_BRANCH_OI_ID%TYPE
        ,P_ORIGEN            IN VARCHAR2)
RETURN GBU_FT_FACTURACION%ROWTYPE
IS
       REG_BUS_ODS                     GBU_FT_FACTURACION%ROWTYPE;
       V_IMPLEMENTACION                CHAR;
BEGIN
     SELECT
            NVL(MAX(DEF.ID_DEFINE),'N') INTO V_IMPLEMENTACION
     FROM   ADAS.GBU_ERROR_TO_DEFINE DEF -- Se utiliza esta tabla de forma temporal.
     WHERE  1=1
     AND    DEF.D_NAME ='CADENA_PEP_MV';

     IF V_IMPLEMENTACION = 'Y' THEN --Si la implementación está realizada llama al New Fn
        REG_BUS_ODS:= CALCULOS_NEW_FN(
                      P_C_BRANCH_OI_ID => P_C_BRANCH_OI_ID
                     ,P_ORIGEN         => P_ORIGEN);
     ELSE--Si la implementación NO está realizada llama al OLD Fn
        REG_BUS_ODS:= CALCULOS_OLD_FN(
                      P_C_BRANCH_OI_ID => P_C_BRANCH_OI_ID
                     ,P_ORIGEN         => P_ORIGEN);
     END IF;

     RETURN REG_BUS_ODS;
END calculos_fn;
-----------------------------------------------------
END GBU_ODS_CALCULOS_PKG;
/
