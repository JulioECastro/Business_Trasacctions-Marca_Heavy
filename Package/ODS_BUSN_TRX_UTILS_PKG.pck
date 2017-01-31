CREATE OR REPLACE PACKAGE ODS_BUSN_TRX_UTILS_PKG
IS
/*********************************************************************************************
Author  :   Pedro Dias (T53605)
Created :   2015-08-20
Purpose :   Administrar los servicios que permitan acceder y disponer de informacion

Historial
Date            Person                  Description
------------    ------------------      -------------------------------------
12/11/2015      Romero Alejandro ()     Creacion de la especificacion del paquete.
-----------------------------------------------------------------------------------------------
*********************************************************************************************/
-- Public type declarations
-- Public constant declarations
        c_other              CONSTANT PLS_INTEGER := 0;
        c_production         CONSTANT PLS_INTEGER := 1;
        c_production_88      CONSTANT PLS_INTEGER := 2;
        c_salesorders        CONSTANT PLS_INTEGER := 3;
        c_purchase           CONSTANT PLS_INTEGER := 4;
        c_replenishment      CONSTANT PLS_INTEGER := 5;
        c_agreement          CONSTANT PLS_INTEGER := 6;
/***************************************************************************************************/
/*******************Funciones para obtener la familia de un documento*******************************/
/***************************************************************************************************/

--////////////////// PUBLIC MODULES SPECIFICATION ////////////////////////

/*********************************************************************************************
Nombre del programa:   GET_FAMILY_ID_FN
        Sistema: ODS Business Transaction Utilities
        Objetivo: Obtener el tipo de familia segun las tablas del ODS de documentos

        Parámetros de entrada:
                p_doc_comm_location_code
                p_document_year
                p_document_month
                p_doc_type_code
                p_document_number
                p_document_item_num
                p_doc_portfolio_type
                p_view

         Parámetros de entrada (SOBRECARGA):
                p_business_id
                p_business_sequence
                p_view

        Parámetros de salida: ods_products.c_family_id%TYPE;
        Id de la familia del producto.

        Notas:
        Autor: Pedro Dias - T53605
        Historia:
        Fecha           Autor                   Descripción
        12/11/2015      Romero Alejandro -      Creacion del procedure
*********************************************************************************************/
FUNCTION get_family_id_fn (
        p_doc_comm_location_code        IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_document_year                 IN ods_business_transactions.document_year%TYPE,
        p_document_month                IN ods_business_transactions.document_month%TYPE,
        p_doc_type_code                 IN ods_business_transactions.doc_type_code%TYPE,
        p_document_number               IN ods_business_transactions.document_number%TYPE,
        p_document_item_num             IN ods_business_transactions.document_item_num%TYPE,
        p_doc_portfolio_type            IN ods_business_transactions.doc_portfolio_type%TYPE)
RETURN ods_products.c_family_id%TYPE;
-------------------------------SOBRECARGA-------------------------------------------
FUNCTION get_family_id_fn (
        p_business_id           IN ods_business_transactions.business_id%TYPE,
        p_business_sequence     IN ods_business_transactions.business_sequence%TYPE)
RETURN ods_products.c_family_id%TYPE;
-----------------------------------------------------------------------------------------------------------------------------

/***************************************************************************************************/
/**************Funciones para obtener la cadena Hexy (main_bough_by_type_flag)**********************/
/***************************************************************************************************/

--////////////////// PUBLIC MODULES SPECIFICATION ////////////////////////

/*********************************************************************************************
Nombre del programa:   IS_EXTORNOS_FN
        Sistema: ODS Business Transaction Utilities
        Objetivo: Determina si un documento es un extorno según la Tabla ODS_BUSN_TRANSACTION_EXTORNOS

        Parámetros de entrada:
                  p_business_id
                  p_doc_comm_location_code
                  p_document_year
                  p_document_month
                  p_doc_type_code
                  p_doc_portfolio_type
                  p_document_number
                  p_document_item_num
                  p_document_split_num

        Parámetros de salida:True/False


        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        28/01/2015      Romero Alejandro -      Creacion del procedure

*********************************************************************************************/
FUNCTION is_extornos_fn (
        p_business_id            IN ods_business_transactions.business_id%TYPE,
        p_doc_comm_location_code IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_document_year          IN ods_business_transactions.document_year%TYPE,
        p_document_month         IN ods_business_transactions.document_month%TYPE,
        p_doc_type_code          IN ods_business_transactions.doc_type_code%TYPE,
        p_doc_portfolio_type     IN ods_business_transactions.doc_portfolio_type%TYPE,
        p_document_number        IN ods_business_transactions.document_number%TYPE,
        p_document_item_num      IN ods_business_transactions.document_item_num%TYPE,
        p_document_split_num     IN ods_business_transactions.document_split_num%TYPE)
RETURN ODS_COMMON_PKG.S_STR_BOOLEAN;
--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   IS_88_OF_COMPLEX_BRANCHES_FN
        Sistema: ODS Business Transaction Utilities
        Objetivo: Determina si un documento es una orden de producción de una filiar compleja
        (Es decir una sociedad que no produce pero emite un documento 88,
         correspondiente a una orden de producción)

        Parámetros de entrada:
                  p_doc_portfolio_type
                  p_doc_comm_location_code

        Parámetros de salida:True/False


        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        29/01/2015      Romero Alejandro -      Creacion del procedure

*********************************************************************************************/
FUNCTION is_88_of_complex_branches_fn(
        p_doc_portfolio_type     IN ods_business_transactions.doc_portfolio_type%TYPE,
        p_doc_comm_location_code IN ods_business_transactions.doc_comm_location_code%TYPE)
RETURN ODS_COMMON_PKG.S_STR_BOOLEAN;
---------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   RECURRENT_WORD_IN_TEXT_FN
        Sistema: ODS Business Transaction Utilities
        Objetivo: Determinar si en un texto pasado por parámetro hay palabras repetidas.
        Se pasa por parámetro el separador de palabras que va a tener el texto pasado por parámetro.
        Parámetros de entrada:
                  p_text (texto a evaluar)
                  p_separator(separador entre palabras dentro del texto a evaluar)

        Parámetros de salida:True/False


        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        29/03/2015      Romero Alejandro -      Creacion del procedure

*********************************************************************************************/
FUNCTION recurrent_word_in_text_fn (
          p_text      IN VARCHAR2
         ,p_separator IN CHAR
          )
RETURN ODS_COMMON_PKG.S_STR_BOOLEAN;
---------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   GET_BETTER_BOUGH_FN
        Sistema: ODS Business Transaction Utilities
        Objetivo:
        Pasado un BID Se obtiene todas las ramas de la cadena y se eliminas los documentos que no son válidos (GET_CLEAN_BOUGH_SP)
        De estas ramas se selecciona la más significativa (GET_BETTER_BOUGH_FN)

        Parámetros de entrada:
                   p_business_id

        Parámetros de salida: ODS_BUSN_TRX_TREE_TABLE_TYPE

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        29/01/2015      Romero Alejandro -      Creacion del procedure
*********************************************************************************************/
FUNCTION get_better_bough_fn (
        p_business_id    IN ods_business_transactions.business_id%TYPE
        )
RETURN ODS_BUSN_TRX_TREE_TABLE_TYPE;

END ods_busn_trx_utils_pkg;
/
CREATE OR REPLACE PACKAGE BODY ODS_BUSN_TRX_UTILS_PKG
IS
/***************************************************************************************************/
/*******************Funciones para obtener la familia de un documento*******************************/
/***************************************************************************************************/

--////////////////// PRIVATE MODULES SPECIFICATION ////////////////////////

/*********************************************************************************************
Nombre del programa:   GET_DOC_PORTFOLIO_FN
        Sistema: ODS Business Transaction Utilities
        Objetivo:

        Parámetros de entrada:
                p_doc_portfolio_type

        Parámetros de salida:


        Notas:
        Autor: Pedro Dias - T53605
        Historia:
        Fecha           Autor                   Descripción
        12/11/2015      Romero Alejandro -      Creacion del procedure
        24/08/2016      Romero Alejandro -      ITTEN00375112 - Modificacion get_clean_boughs_sp (Se agregque la intermil no tenga un postproceso)
*********************************************************************************************/
--------------------------------------------------------------------------------
FUNCTION get_doc_portfolio_fn (
        p_doc_portfolio_type    IN ods_business_transactions.doc_portfolio_type%TYPE)
RETURN PLS_INTEGER
IS
        v_result        PLS_INTEGER;
BEGIN
        v_result := CASE
                        WHEN p_doc_portfolio_type IN ('04','06','70')   THEN c_production
                        WHEN p_doc_portfolio_type IN ('88')             THEN c_production_88
                        WHEN p_doc_portfolio_type IN ('02','41')        THEN c_salesorders
                        WHEN p_doc_portfolio_type IN ('01','40')        THEN c_purchase
                        WHEN p_doc_portfolio_type IN ('90')             THEN c_replenishment
                        WHEN p_doc_portfolio_type IN ('50')             THEN c_agreement
                        ELSE c_other
                    END;
        RETURN v_result;
END get_doc_portfolio_fn;
--------------------------------------------------------------------------------

--////////////////// PUBLIC MODULES IMPLEMENTATION ////////////////////////
FUNCTION get_family_id_fn (
        p_doc_comm_location_code        IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_document_year                 IN ods_business_transactions.document_year%TYPE,
        p_document_month                IN ods_business_transactions.document_month%TYPE,
        p_doc_type_code                 IN ods_business_transactions.doc_type_code%TYPE,
        p_document_number               IN ods_business_transactions.document_number%TYPE,
        p_document_item_num             IN ods_business_transactions.document_item_num%TYPE,
        p_doc_portfolio_type            IN ods_business_transactions.doc_portfolio_type%TYPE)
RETURN ods_products.c_family_id%TYPE
IS
        v_result                ods_products.c_family_id%TYPE;
        v_doc_portfolio         ods_business_transactions.doc_portfolio_type%TYPE;
        v_doc_type_code         ods_business_transactions.doc_type_code%TYPE;
BEGIN
        v_doc_portfolio := get_doc_portfolio_fn (p_doc_portfolio_type);
        v_doc_type_code := TRIM (p_doc_type_code);

        IF v_doc_portfolio = c_production
        THEN
               SELECT  MAX (p.c_family_id)
               INTO    v_result
               FROM    ods_production_orders_itm pro,  --Vista ods_production_orders_itm_vw                                                                                                                                      --Utiliza la tabla pura
                       ods_products p
               WHERE   1 = 1
               AND     pro.product_code = p.c_product_id
               AND     pro.commercial_location_code = p_doc_comm_location_code
               AND     pro.pro_order_year = p_document_year
               AND     pro.pro_order_month = p_document_month
               AND     TRIM (pro.doc_type_code) = v_doc_type_code
               AND     pro.pro_number = p_document_number
               AND     pro.pro_item_num = p_document_item_num;

        ELSIF v_doc_portfolio = c_production_88
        THEN

               SELECT  MAX (p.c_family_id)
               INTO    v_result
               FROM    ods_production_orders_itm pro,  --Vista ods_production_orders_itm_vw                                                                                                                                      --Utiliza la tabla pura
                       ods_products p
               WHERE   1 = 1
               AND     pro.product_code = p.c_product_id
               AND     pro.commercial_location_code = p_doc_comm_location_code
               AND     pro.pro_item_year = p_document_year
               AND     pro.pro_item_month = p_document_month
               AND     TRIM (pro.doc_type_code) = v_doc_type_code
               AND     pro.pro_number = p_document_number
               AND     pro.pro_item_num = p_document_item_num;

        ELSIF v_doc_portfolio = c_salesorders
        THEN
               SELECT  MAX (p.c_family_id)
               INTO    v_result
               FROM    ods_sale_orders_itm s,    --Vista ods_prod_schema_so_itm_wop_vw                                                                                                                                             --Utiliza la tabla pura
                       ods_products p
               WHERE   1 = 1
               AND     s.product_code = p.c_product_id
               AND     s.commercial_location_code = p_doc_comm_location_code
               AND     s.so_order_year = p_document_year
               AND     s.so_order_month = p_document_month
               AND     TRIM (s.doc_type_code) = v_doc_type_code
               AND     s.so_number = p_document_number
               AND     s.so_item_num = p_document_item_num;

        ELSIF v_doc_portfolio = c_purchase
        THEN
                SELECT  MAX (p.c_family_id)
                INTO    v_result
                FROM    ods_purchase_orders_itm pur,
                        ods_products p
                WHERE   1 = 1
                AND     pur.product_code = p.c_product_id
                AND     pur.commercial_location_code = p_doc_comm_location_code
                AND     pur.po_order_year = p_document_year
                AND     pur.po_order_month = p_document_month
                AND     TRIM (pur.doc_type_code) = v_doc_type_code
                AND     pur.po_number = p_document_number
                AND     pur.po_item_num = p_document_item_num;

        ELSIF v_doc_portfolio = c_replenishment
        THEN
                SELECT  MAX (p.c_family_id)
                INTO    v_result
                FROM    ods_replenishment_orders_itm ro,
                        ods_products p
                WHERE  1 = 1
                AND    ro.product_code = p.c_product_id
                AND    ro.commercial_location_code = p_doc_comm_location_code
                AND    ro.ro_order_year = p_document_year
                AND    ro.ro_order_month = p_document_month
                AND    TRIM (ro.ro_doc_type_code) = v_doc_type_code
                AND    ro.ro_number = p_document_number
                AND    ro.ro_item_num = p_document_item_num;

        ELSIF v_doc_portfolio = c_agreement
        THEN
                SELECT  MAX (p.c_family_id)
                INTO    v_result
                FROM    ods_agreements_itm a,
                        ods_products p
                WHERE   1 = 1
                AND     a.product_code = p.c_product_id
                AND     a.agreement_year = p_document_year
                AND     TRIM (a.agreement_doc_type_code) = v_doc_type_code
                AND     a.agreement_number = p_document_number
                AND     a.agreement_item_num = p_document_item_num;

        ELSE
                v_result := NULL;
        END IF;

        RETURN v_result;
END get_family_id_fn;
-------------------------------SOBRECARGA-------------------------------------------
FUNCTION get_family_id_fn (
        p_business_id           IN ods_business_transactions.business_id%TYPE,
        p_business_sequence     IN ods_business_transactions.business_sequence%TYPE)
RETURN ods_products.c_family_id%TYPE
IS
        v_result  ods_products.c_family_id%TYPE;
BEGIN
        SELECT MAX (get_family_id_fn (bt.doc_comm_location_code,
                           bt.document_year,
                           bt.document_month,
                           bt.doc_type_code,
                           bt.document_number,
                           bt.document_item_num,
                           bt.doc_portfolio_type))
        INTO    v_result
        FROM    ods_business_transactions bt
        WHERE   1 = 1
        AND     bt.business_id = p_business_id
        AND     bt.business_sequence = p_business_sequence;

        RETURN v_result;
END get_family_id_fn;

/***************************************************************************************************/
/**************Funciones para obtener la cadena Hexy (main_bough_by_type_flag)**********************/
/***************************************************************************************************/
/***************************************************************************************************/
--////////////////// PUBLIC MODULES IMPLEMENTATION ////////////////////////
FUNCTION is_extornos_fn (
        p_business_id            IN ods_business_transactions.business_id%TYPE,
        p_doc_comm_location_code IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_document_year          IN ods_business_transactions.document_year%TYPE,
        p_document_month         IN ods_business_transactions.document_month%TYPE,
        p_doc_type_code          IN ods_business_transactions.doc_type_code%TYPE,
        p_doc_portfolio_type     IN ods_business_transactions.doc_portfolio_type%TYPE,
        p_document_number        IN ods_business_transactions.document_number%TYPE,
        p_document_item_num      IN ods_business_transactions.document_item_num%TYPE,
        p_document_split_num     IN ods_business_transactions.document_split_num%TYPE)
RETURN ODS_COMMON_PKG.S_STR_BOOLEAN
IS
        v_result  ODS_COMMON_PKG.S_STR_BOOLEAN;
BEGIN
     BEGIN
          SELECT ODS_COMMON_PKG.C_STR_TRUE INTO v_result
          FROM   ADAS.ODS_BUSN_TRANSACTION_EXTORNOS EX
          WHERE  1=1
          AND    p_doc_type_code              = '00'
          AND    EX.DOC_TYPE_CODE             = 'IM'
          AND    EX.BUSINESS_ID               = p_business_id
          AND    EX.PARENT_DOC_COMM_LOC_CODE  = p_doc_comm_location_code
          AND    EX.PARENT_DOC_YEAR           = p_document_year
          AND    EX.PARENT_DOC_MONTH          = p_document_month
          AND    EX.PARENT_DOC_PORTFOLIO_TYPE = p_doc_portfolio_type
          AND    EX.PARENT_DOC_NUM            = p_document_number
          AND    EX.PARENT_DOC_ITEM_NUM       = p_document_item_num
          AND    EX.PARENT_DOC_SPLIT_NUM      = p_document_split_num
          AND    ROWNUM=1;

          EXCEPTION
	        WHEN NO_DATA_FOUND THEN
		           v_result := ODS_COMMON_PKG.C_STR_FALSE;
     END;

     RETURN v_result;
END is_extornos_fn;
--------------------------------------------------------------------------------

FUNCTION is_88_of_complex_branches_fn(
        p_doc_portfolio_type     IN ods_business_transactions.doc_portfolio_type%TYPE,
        p_doc_comm_location_code IN ods_business_transactions.doc_comm_location_code%TYPE)
RETURN ODS_COMMON_PKG.S_STR_BOOLEAN
IS
        v_result   ODS_COMMON_PKG.S_STR_BOOLEAN;
BEGIN
        v_result   := ODS_COMMON_PKG.C_STR_FALSE;

        IF (p_doc_portfolio_type = '88')
           AND
           (p_doc_comm_location_code IN ('SPI','TSA','KPT'))
        THEN
              v_result := ODS_COMMON_PKG.C_STR_TRUE;
        END IF;

        RETURN v_result;

END is_88_of_complex_branches_fn;
--------------------------------------------------------------------------------

FUNCTION recurrent_word_in_text_fn (
           p_text      IN VARCHAR2
          ,p_separator IN CHAR
          )
RETURN   ODS_COMMON_PKG.S_STR_BOOLEAN
IS
        v_result    ODS_COMMON_PKG.S_STR_BOOLEAN;
        v_pos_word  PLS_INTEGER;
        v_text      VARCHAR2(4000);
        v_word      VARCHAR2(4000);
BEGIN
     v_result    := ODS_COMMON_PKG.C_STR_FALSE;
     /*Para que el algoritmo funcione correctamente precisa tener el separador de palabras (p_separator)
     al inicio y al final del texto.
     Como no sabemos si el separador esta solo al final o solo al inicio, ni al final ni al inicio
     o en los dos lados. Se quita el separador del inicio y el final si lo tuviera y  luego se
     lo coloca al inicio y al final.*/
     v_text      := p_separator || TRIM(p_separator FROM p_text)  || p_separator;

     /*La variable v_pos_word nos indica donde finaliza, según el separador, la primer palabra.
     Por esto comienza a buscar desde la posición 2, ya que la primera posición tiene el carácter
     separador*/
     v_pos_word  := NVL(INSTR(v_text, p_separator,2),0);

      WHILE (v_pos_word > 0) LOOP--Recorre mientras el texto tenga palabras.
	         v_word:= SUBSTR(v_text,1,v_pos_word);--se queda con la primer palabra del texto.
           v_text:= SUBSTR(v_text,v_pos_word,LENGTH(p_text));--al texto le saca la primer palabra

           /*Evaluó si la primer palabra existe en el texto*/
           IF ((LENGTH(v_text)-LENGTH(REPLACE(v_text,v_word,'')))/LENGTH(v_word)) >= 1 THEN
               v_result := ODS_COMMON_PKG.C_STR_TRUE;
               EXIT;--si la palabra existe sale del loop
           END IF;

           v_pos_word:= NVL(INSTR(v_text, p_separator,2),0);--obtiene donde finaliza la próxima palabra, si la hubiera.
      END LOOP;
     RETURN v_result;
END recurrent_word_in_text_fn;
--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   ADD_DATA_SP
        Sistema: ODS Business Transaction Utilities
        Objetivo: Pasado por parámetros dos variable (TYPE_TABLE) una IN y otra OUT
        Retorna el contenido de la variable IN agregado al contenido de la variable OUT

        Parámetros de entrada/Salida:
            p_busn_trx_tree_IN          ODS_BUSN_TRX_TREE_TABLE_TYPE (tabla temp donde se tiene valores a agregar a tabla tem OUT)
            p_busn_trx_tree_OUT         ODS_BUSN_TRX_TREE_TABLE_TYPE (tabla temp donde se van acumulando los valores de IN)
        Parámetros de salida: --

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        29/01/2015      Romero Alejandro -      Creacion del procedure
*********************************************************************************************/
PROCEDURE add_data_sp (
          p_busn_trx_tree_IN        IN OUT NOCOPY ODS_BUSN_TRX_TREE_TABLE_TYPE
         ,p_busn_trx_tree_OUT       IN OUT NOCOPY ODS_BUSN_TRX_TREE_TABLE_TYPE
          )
IS
        /*Recorro los registros de la variable IN*/
        CURSOR cur_in_data
        IS
        SELECT
                BT.*
        FROM
                TABLE(CAST(p_busn_trx_tree_IN AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT;
        ---
        v_in_data_rec            cur_in_data%ROWTYPE;
BEGIN
      /*Recorro cursor con registros de variable IN y los agrego en Variable OUT*/
      FOR v_in_data_rec IN cur_in_data
      LOOP
           p_busn_trx_tree_OUT.EXTEND;
           p_busn_trx_tree_OUT(p_busn_trx_tree_OUT.COUNT) :=
                      ODS_BUSN_TRX_TREE_ROW_TYPE(
                           v_in_data_rec.IS_LEAF
                          ,v_in_data_rec.LEAF_PATH_DESC
                          ,v_in_data_rec.PATH_DESC
                          ,v_in_data_rec.ORIGINAL_BUSINESS_ID
                          ,v_in_data_rec.ORIGINAL_BUSINESS_SEQUENCE
                          ,v_in_data_rec.BUSINESS_ID
                          ,v_in_data_rec.BUSINESS_SEQUENCE
                          ,v_in_data_rec.BUSINESS_REFERENCE_ID
                          ,v_in_data_rec.BUSINESS_REFERENCE_SEQ
                          ,v_in_data_rec.DOC_COMM_LOCATION_CODE
                          ,v_in_data_rec.DOCUMENT_YEAR
                          ,v_in_data_rec.DOCUMENT_MONTH
                          ,v_in_data_rec.DOC_TYPE_CODE
                          ,v_in_data_rec.DOC_PORTFOLIO_TYPE
                          ,v_in_data_rec.DOCUMENT_NUMBER
                          ,v_in_data_rec.DOCUMENT_ITEM_NUM
                          ,v_in_data_rec.DOCUMENT_SPLIT_NUM
                          ,v_in_data_rec.FAMILY_TYPE_REF
                          );
      END LOOP;
END add_data_sp;
--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   GET_FULL_BUSN_TRX_TREE_FN
        Sistema: ODS Business Transaction Utilities
        Objetivo: Pasando un BID obtiene , recursivamente, todos los BID intervinientes
        en el negocio y que pertenezcan al tipo de producto (p_family_type_ref) pasado por parámetro.

        Parámetros de entrada:
                  p_business_id
                  p_family_type_ref

        Parámetros de entrada/salida: ODS_BUSN_TRX_TREE_TABLE_TYPE


        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        29/01/2015      Romero Alejandro -      Creacion del procedure

*********************************************************************************************/

PROCEDURE get_full_busn_trx_tree_sp (
        p_business_id                IN ods_business_transactions.business_id%TYPE
       ,p_family_type_ref            IN ODS_BSN_DOC_TREE_FAMILY_RANK.FAMILY_TYPE_REF%TYPE
       ,p_busn_trx_tree_result_table IN OUT NOCOPY ODS_BUSN_TRX_TREE_TABLE_TYPE
        )
IS
BEGIN
     BEGIN
                SELECT  ODS_BUSN_TRX_TREE_ROW_TYPE(
                        X.IS_LEAF
                       ,X.LEAF_PATH_DESC
                       ,X.PATH_DESC
                       ,X.ORIGINAL_BUSINESS_ID
                       ,X.ORIGINAL_BUSINESS_SEQUENCE
                       ,X.BUSINESS_ID
                       ,X.BUSINESS_SEQUENCE
                       ,X.BUSINESS_REFERENCE_ID
                       ,X.BUSINESS_REFERENCE_SEQ
                       ,X.DOC_COMM_LOCATION_CODE
                       ,X.DOCUMENT_YEAR
                       ,X.DOCUMENT_MONTH
                       ,X.DOC_TYPE_CODE
                       ,X.DOC_PORTFOLIO_TYPE
                       ,X.DOCUMENT_NUMBER
                       ,X.DOCUMENT_ITEM_NUM
                       ,X.DOCUMENT_SPLIT_NUM
                       ,X.FAMILY_TYPE_REF) BULK COLLECT INTO p_busn_trx_tree_result_table
                FROM(
                       SELECT
                              NULL                                    AS IS_LEAF
                             ,NULL                                    AS LEAF_PATH_DESC
                             ,SYS_CONNECT_BY_PATH(BT.BUSINESS_ID,'.') AS PATH_DESC
                             ,CONNECT_BY_ROOT BT.BUSINESS_ID          AS ORIGINAL_BUSINESS_ID
                             ,CONNECT_BY_ROOT BT.BUSINESS_SEQUENCE    AS ORIGINAL_BUSINESS_SEQUENCE
                             ,BT.BUSINESS_ID
                             ,BT.BUSINESS_SEQUENCE
                             ,BT.BUSINESS_REFERENCE_ID
                             ,BT.BUSINESS_REFERENCE_SEQ
                             ,BT.DOC_COMM_LOCATION_CODE
                             ,BT.DOCUMENT_YEAR
                             ,BT.DOCUMENT_MONTH
                             ,BT.DOC_TYPE_CODE
                             ,BT.DOC_PORTFOLIO_TYPE
                             ,BT.DOCUMENT_NUMBER
                             ,BT.DOCUMENT_ITEM_NUM
                             ,BT.DOCUMENT_SPLIT_NUM
                             ,F.FAMILY_TYPE_REF
                       FROM
                              ADAS.ODS_BUSINESS_TRANSACTIONS BT
                             ,ADAS.ODS_BSN_DOC_TREE_FAMILY_RANK F
                       WHERE  1=1
                       AND    BT.HISTORICAL_LINK_FLAG = 'N'
                       AND    BT.DOCUMENT_FAMILY_CODE IS NOT NULL
                       AND    F.FAMILY_CODE           = BT.DOCUMENT_FAMILY_CODE
                       AND    (
                              BT.BUSINESS_REFERENCE_ID <> 9999999999
                              OR
                              BT.BUSINESS_REFERENCE_ID IS NULL
                              )
                       CONNECT BY NOCYCLE
                               BT.BUSINESS_ID            = PRIOR BT.BUSINESS_REFERENCE_ID
                           AND BT.BUSINESS_SEQUENCE      = PRIOR BT.BUSINESS_REFERENCE_SEQ
                           AND BT.DOC_COMM_LOCATION_CODE = PRIOR BT.DOC_COMM_LOCATION_CODE
                           AND BT.DOCUMENT_YEAR          = PRIOR BT.DOCUMENT_YEAR
                           AND BT.DOCUMENT_MONTH         = PRIOR BT.DOCUMENT_MONTH
                           AND BT.DOC_TYPE_CODE          = PRIOR BT.DOC_TYPE_CODE
                           AND BT.DOC_PORTFOLIO_TYPE     = PRIOR BT.DOC_PORTFOLIO_TYPE
                           AND BT.DOCUMENT_NUMBER        = PRIOR BT.DOCUMENT_NUMBER
                           AND BT.DOCUMENT_ITEM_NUM      = PRIOR BT.DOCUMENT_ITEM_NUM
                           AND BT.DOCUMENT_SPLIT_NUM     = PRIOR BT.DOCUMENT_SPLIT_NUM
                           AND BT.HISTORICAL_LINK_FLAG   = PRIOR BT.HISTORICAL_LINK_FLAG
                           AND F.FAMILY_TYPE_REF         = PRIOR F.FAMILY_TYPE_REF
                       START WITH BT.BUSINESS_ID         = p_business_id
                             AND  F.FAMILY_TYPE_REF      = p_family_type_ref
                    )X
                    WHERE  1=1
                    AND    RECURRENT_WORD_IN_TEXT_FN(X.PATH_DESC,'.') = ODS_COMMON_PKG.C_STR_FALSE;

     EXCEPTION
              WHEN NO_DATA_FOUND THEN
              p_busn_trx_tree_result_table.DELETE;

              WHEN OTHERS THEN
              p_busn_trx_tree_result_table.DELETE;
     END;
END get_full_busn_trx_tree_sp;

--////////////////// PRIVATE MODULES SPECIFICATION AND PUBLIC por SOBRECARGA////////////////////////
--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   GET_LEAF_OF_BUSN_TRX_TREE_SP
        Sistema: ODS Business Transaction Utilities
        Objetivo: Pasando el conjunto de cadenas intervinientes de un BID se obtiene cuáles son
        las hojas (IS_LEAF = Y) de las ramas de todo el BID. y la cantidad de hojas para el Bid

        Parámetros de entrada / Salida:
                  ODS_BUSN_TRX_TREE_TABLE_TYPE
                  p_count_leaf

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        29/01/2015      Romero Alejandro -      Creacion del procedure
*********************************************************************************************/
PROCEDURE get_leaf_of_busn_trx_tree_sp (
        p_busn_trx_tree_table    IN OUT NOCOPY ODS_BUSN_TRX_TREE_TABLE_TYPE
       ,p_count_leaf             IN OUT PLS_INTEGER
        )
IS
        CURSOR cur_leaf_of_busn_trx_tree
        IS
        SELECT
               /*Si para el BID donde estoy parados no existe una parte referenciada, es una hoja.
               Adicionalmente le agrego que el BSecuence sea igual al máximo que tiene el BID,
               así de esta manera solo marco (IS_LEAF = Y) un solo registro como hoja.
               Dado que si marco más de uno (si existiera), es innecesario para la lógica y me duplicaría los resultados*/
               CASE WHEN (   --Si es hoja
                             SELECT MAX(1)
                             FROM
                                    TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT1
                             WHERE 1=1
                             AND   BT1.BUSINESS_ID             = BT.BUSINESS_ID
                             AND   BT1.BUSINESS_REFERENCE_ID IS NOT NULL
                          ) IS NULL
                          AND
                         (--Si es el Bsecuence máximo  (solo para marcar un solo registro)
                             SELECT MAX(BT1.BUSINESS_SEQUENCE)
                             FROM
                                    TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT1
                             WHERE 1=1
                             AND   BT1.BUSINESS_ID = BT.BUSINESS_ID
                          ) = BT.BUSINESS_SEQUENCE
               THEN 'Y'
               ELSE ''
               END AS IS_LEAF
              ,BT.LEAF_PATH_DESC
              ,BT.PATH_DESC
              ,BT.ORIGINAL_BUSINESS_ID
              ,BT.ORIGINAL_BUSINESS_SEQUENCE
              ,BT.BUSINESS_ID
              ,BT.BUSINESS_SEQUENCE
              ,BT.BUSINESS_REFERENCE_ID
              ,BT.BUSINESS_REFERENCE_SEQ
              ,BT.DOC_COMM_LOCATION_CODE
              ,BT.DOCUMENT_YEAR
              ,BT.DOCUMENT_MONTH
              ,BT.DOC_TYPE_CODE
              ,BT.DOC_PORTFOLIO_TYPE
              ,BT.DOCUMENT_NUMBER
              ,BT.DOCUMENT_ITEM_NUM
              ,BT.DOCUMENT_SPLIT_NUM
              ,BT.FAMILY_TYPE_REF
        FROM
               TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT;

        v_leaf_of_busn_trx_tree_rec           cur_leaf_of_busn_trx_tree%ROWTYPE;
        v_First BOOLEAN;
BEGIN
     v_First      := TRUE;
     p_count_leaf := 0;
     BEGIN
        /*Utilizo un cursor en lugar de una BULK COLLECT INTO  para poder contabilizar los
        registros que son hojas en el parámetro (IN OUT) p_count_leaf*/
        FOR v_leaf_of_busn_trx_tree_rec IN cur_leaf_of_busn_trx_tree
        LOOP

             /*Si no limpio la variable de Type_Table se añadirían a lo que ya está.
             Por eso la primera vez que voy a ingresar datos la limpio.*/
             IF v_First  THEN
                    p_busn_trx_tree_table.DELETE;
                    v_First:= FALSE;
             END IF;

             /*contabilizo las hojas*/
             IF v_leaf_of_busn_trx_tree_rec.IS_LEAF = 'Y' THEN
                    p_count_leaf:= p_count_leaf +1;
             END iF;

             p_busn_trx_tree_table.EXTEND;
             p_busn_trx_tree_table(p_busn_trx_tree_table.COUNT) :=
             ODS_BUSN_TRX_TREE_ROW_TYPE(
                               v_leaf_of_busn_trx_tree_rec.IS_LEAF
                              ,v_leaf_of_busn_trx_tree_rec.LEAF_PATH_DESC
                              ,v_leaf_of_busn_trx_tree_rec.PATH_DESC
                              ,v_leaf_of_busn_trx_tree_rec.ORIGINAL_BUSINESS_ID
                              ,v_leaf_of_busn_trx_tree_rec.ORIGINAL_BUSINESS_SEQUENCE
                              ,v_leaf_of_busn_trx_tree_rec.BUSINESS_ID
                              ,v_leaf_of_busn_trx_tree_rec.BUSINESS_SEQUENCE
                              ,v_leaf_of_busn_trx_tree_rec.BUSINESS_REFERENCE_ID
                              ,v_leaf_of_busn_trx_tree_rec.BUSINESS_REFERENCE_SEQ
                              ,v_leaf_of_busn_trx_tree_rec.DOC_COMM_LOCATION_CODE
                              ,v_leaf_of_busn_trx_tree_rec.DOCUMENT_YEAR
                              ,v_leaf_of_busn_trx_tree_rec.DOCUMENT_MONTH
                              ,v_leaf_of_busn_trx_tree_rec.DOC_TYPE_CODE
                              ,v_leaf_of_busn_trx_tree_rec.DOC_PORTFOLIO_TYPE
                              ,v_leaf_of_busn_trx_tree_rec.DOCUMENT_NUMBER
                              ,v_leaf_of_busn_trx_tree_rec.DOCUMENT_ITEM_NUM
                              ,v_leaf_of_busn_trx_tree_rec.DOCUMENT_SPLIT_NUM
                              ,v_leaf_of_busn_trx_tree_rec.FAMILY_TYPE_REF );
        END LOOP;
        IF p_busn_trx_tree_table.COUNT = 0 THEN
           RAISE NO_DATA_FOUND;
        END IF;

     EXCEPTION
              WHEN NO_DATA_FOUND THEN
              p_busn_trx_tree_table.DELETE;
              p_count_leaf:=0;
              WHEN OTHERS THEN
              p_busn_trx_tree_table.DELETE;
              p_count_leaf:=0;
     END;
END get_leaf_of_busn_trx_tree_sp;
----------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   GET_BOUGH_VIA_LEAF_SP
        Sistema: ODS Business Transaction Utilities
        Objetivo: Pasando el conjunto de cadenas intervinientes de un BID y las Hojas (IS_LEAF = Y)
        se obtiene de forma recursiva todas las ramas de todo el BID desde la Hoja hasta la raíz.

        Parámetros de entrada/Salida:
                  ODS_BUSN_TRX_TREE_TABLE_TYPE

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        29/01/2015      Romero Alejandro -      Creacion del procedure
*********************************************************************************************/
PROCEDURE get_bough_via_leaf_sp (
        p_busn_trx_tree_table    IN OUT NOCOPY ODS_BUSN_TRX_TREE_TABLE_TYPE
        )
IS
       v_leaf_of_tree_result_table  ODS_BUSN_TRX_TREE_TABLE_TYPE:= ODS_BUSN_TRX_TREE_TABLE_TYPE();
BEGIN
    BEGIN

       SELECT  ODS_BUSN_TRX_TREE_ROW_TYPE(
               BT1.IS_LEAF
              ,BOUGH_OF_LEAF.LEAF_PATH_DESC
              ,BT1.PATH_DESC
              ,BT1.ORIGINAL_BUSINESS_ID
              ,BT1.ORIGINAL_BUSINESS_SEQUENCE
              ,BT1.BUSINESS_ID
              ,BT1.BUSINESS_SEQUENCE
              ,BT1.BUSINESS_REFERENCE_ID
              ,BT1.BUSINESS_REFERENCE_SEQ
              ,BT1.DOC_COMM_LOCATION_CODE
              ,BT1.DOCUMENT_YEAR
              ,BT1.DOCUMENT_MONTH
              ,BT1.DOC_TYPE_CODE
              ,BT1.DOC_PORTFOLIO_TYPE
              ,BT1.DOCUMENT_NUMBER
              ,BT1.DOCUMENT_ITEM_NUM
              ,BT1.DOCUMENT_SPLIT_NUM
              ,BT1.FAMILY_TYPE_REF   ) BULK COLLECT INTO v_leaf_of_tree_result_table
        FROM
               TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE))BT1,
              (      /*Obtengo recursivamente todos los negocios intervinientes en las hojas.*/
                     SELECT
                            CONNECT_BY_ROOT BT.PATH_DESC AS LEAF_PATH_DESC
                           ,BT.PATH_DESC
                           ,BT.BUSINESS_ID
                     FROM
                            TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT
                     CONNECT BY NOCYCLE
                             BT.BUSINESS_REFERENCE_ID      = PRIOR BT.BUSINESS_ID
                     AND     BT.BUSINESS_REFERENCE_SEQ     = PRIOR BT.BUSINESS_SEQUENCE
                     AND     BT.ORIGINAL_BUSINESS_SEQUENCE = PRIOR BT.ORIGINAL_BUSINESS_SEQUENCE

                     START WITH BT.IS_LEAF ='Y'
              )BOUGH_OF_LEAF
        /*Me quedo con las partes puras (Breferences is null) pertenecientes a la rama de la hoja.
          de esta manera armo la rama completa para una hoja determinada*/
        WHERE 1=1
        AND   BT1.BUSINESS_REFERENCE_ID IS NULL
        AND   BT1.BUSINESS_ID       = BOUGH_OF_LEAF.BUSINESS_ID
        AND   BT1.PATH_DESC         = BOUGH_OF_LEAF.PATH_DESC;

     EXCEPTION
              WHEN NO_DATA_FOUND THEN
              p_busn_trx_tree_table.DELETE;

              WHEN OTHERS THEN
              p_busn_trx_tree_table.DELETE;
     END;

     p_busn_trx_tree_table.DELETE;--Limpio la variable de salida.
     p_busn_trx_tree_table:=v_leaf_of_tree_result_table;--A la variable de Salida le asigno el valor de la variable calculada
     v_leaf_of_tree_result_table.DELETE;--Limpio la variable calculada

END get_bough_via_leaf_sp;
--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   GET_LEAFLESS_BOUGH_SP
        Sistema: ODS Business Transaction Utilities
        Objetivo: Pasando el conjunto de cadenas intervinientes de un BID
        se obtiene los ID de Secuencia referenciada como ramas del BID.
        Esto se utiliza cuando las cadenas están mal armadas y no se pueden obtener las hojas.
        (o la cantidad de registros o Ramas son excesivas)
        Se simulan las referencias como ramas.

        Parámetros de entrada/Salida:
                  ODS_BUSN_TRX_TREE_TABLE_TYPE

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        29/01/2015      Romero Alejandro -      Creacion del procedure
*********************************************************************************************/
PROCEDURE get_leafless_bough_sp (
        p_busn_trx_tree_table    IN OUT NOCOPY ODS_BUSN_TRX_TREE_TABLE_TYPE
        )
IS
        /*Cursor para obtener todos los BId Reference del BID Principal*/
        CURSOR cur_reference_id
        IS
        SELECT
               DISTINCT BT.BUSINESS_REFERENCE_ID AS BUSINESS_REFERENCE_ID
        FROM
               TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT
        WHERE 1=1
        AND   BT.ORIGINAL_BUSINESS_ID = BT.BUSINESS_ID
        AND   BT.BUSINESS_REFERENCE_ID IS NOT NULL;

        /*Cursor para obtener la parte pura (sin Referencia) del BID Principal*/
        CURSOR cur_principal_id
        IS
        SELECT BT.*
        FROM
               TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE))BT
               WHERE 1=1
               AND   BT.ORIGINAL_BUSINESS_ID = BT.BUSINESS_ID
               AND   BT.BUSINESS_REFERENCE_ID IS NULL;

        v_cur_reference_id_rec          cur_reference_id%ROWTYPE;
        v_cur_principal_id_rec          cur_principal_id%ROWTYPE;
        v_leaf_of_tree_result_table     ODS_BUSN_TRX_TREE_TABLE_TYPE:= ODS_BUSN_TRX_TREE_TABLE_TYPE();
BEGIN
    BEGIN
        /*Cargo en la variable de resultado, todos los BId con referencia(no puros) del BID Principal
        y coloco como ruta de la hoja (LEAF_PATH_DESC) el valor de BidReference
        De esta manera por cada BidReference armo una rama.*/
        SELECT  ODS_BUSN_TRX_TREE_ROW_TYPE(
                           BT.IS_LEAF
                          ,BT.BUSINESS_REFERENCE_ID  --AS LEAF_PATH_DESC
                          ,BT.PATH_DESC
                          ,BT.ORIGINAL_BUSINESS_ID
                          ,BT.ORIGINAL_BUSINESS_SEQUENCE
                          ,BT.BUSINESS_ID
                          ,BT.BUSINESS_SEQUENCE
                          ,BT.BUSINESS_REFERENCE_ID
                          ,BT.BUSINESS_REFERENCE_SEQ
                          ,BT.DOC_COMM_LOCATION_CODE
                          ,BT.DOCUMENT_YEAR
                          ,BT.DOCUMENT_MONTH
                          ,BT.DOC_TYPE_CODE
                          ,BT.DOC_PORTFOLIO_TYPE
                          ,BT.DOCUMENT_NUMBER
                          ,BT.DOCUMENT_ITEM_NUM
                          ,BT.DOCUMENT_SPLIT_NUM
                          ,BT.FAMILY_TYPE_REF) BULK COLLECT INTO v_leaf_of_tree_result_table
        FROM
        TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE))BT
        WHERE 1=1
        AND   BT.ORIGINAL_BUSINESS_ID = BT.BUSINESS_ID
        AND   BT.BUSINESS_REFERENCE_ID IS NOT NULL;

        /*Recorro todos los BidRFeference del Bid Principal */
        FOR v_cur_reference_id_rec IN cur_reference_id
        LOOP
               /*Por cada BidReference del Bid principal, completo la variable de resultado
               con la parte pura del bid Principal y coloco como ruta de la hoja (LEAF_PATH_DESC)
               el valor de BidReference.
               De esta manera termino de completar (con la parte pura del bid principal)
               la rama completa para cada BidReference.*/
               FOR v_cur_principal_id_rec IN cur_principal_id
               LOOP
                     v_leaf_of_tree_result_table.EXTEND;
                     v_leaf_of_tree_result_table(v_leaf_of_tree_result_table.COUNT) :=
                          ODS_BUSN_TRX_TREE_ROW_TYPE(
                               v_cur_principal_id_rec.IS_LEAF
                              ,v_cur_reference_id_rec.BUSINESS_REFERENCE_ID --AS LEAF_PATH_DESC
                              ,v_cur_principal_id_rec.PATH_DESC
                              ,v_cur_principal_id_rec.ORIGINAL_BUSINESS_ID
                              ,v_cur_principal_id_rec.ORIGINAL_BUSINESS_SEQUENCE
                              ,v_cur_principal_id_rec.BUSINESS_ID
                              ,v_cur_principal_id_rec.BUSINESS_SEQUENCE
                              ,v_cur_principal_id_rec.BUSINESS_REFERENCE_ID
                              ,v_cur_principal_id_rec.BUSINESS_REFERENCE_SEQ
                              ,v_cur_principal_id_rec.DOC_COMM_LOCATION_CODE
                              ,v_cur_principal_id_rec.DOCUMENT_YEAR
                              ,v_cur_principal_id_rec.DOCUMENT_MONTH
                              ,v_cur_principal_id_rec.DOC_TYPE_CODE
                              ,v_cur_principal_id_rec.DOC_PORTFOLIO_TYPE
                              ,v_cur_principal_id_rec.DOCUMENT_NUMBER
                              ,v_cur_principal_id_rec.DOCUMENT_ITEM_NUM
                              ,v_cur_principal_id_rec.DOCUMENT_SPLIT_NUM
                              ,v_cur_principal_id_rec.FAMILY_TYPE_REF
                              );
               END LOOP;
        END LOOP;

        IF v_leaf_of_tree_result_table.COUNT = 0 THEN
           RAISE NO_DATA_FOUND;
        END IF;

     EXCEPTION
              WHEN NO_DATA_FOUND THEN
              v_leaf_of_tree_result_table.DELETE;

              WHEN OTHERS THEN
              v_leaf_of_tree_result_table.DELETE;
    END;
    p_busn_trx_tree_table.DELETE;--Limpio la variable de salida.
    p_busn_trx_tree_table:= v_leaf_of_tree_result_table;--A la variable de Salida le asigno el valor de la variable calculada
    v_leaf_of_tree_result_table.DELETE;--Limpio la variable calculada
END get_leafless_bough_sp;
--------------------------------------------------------------------------------

/*********************************************************************************************
Nombre del programa:   GET_CLEAN_BOUGH_SP
        Sistema: ODS Business Transaction Utilities
        Objetivo: Pasando el conjunto de cadenas intervinientes de un BID donde ya están armadas las ramas de la cadena.
        Se limpian (eliminan) los documentos de producción que son extornos (IS_EXTORNOS_FN).
        También se elimina los documentos de producción si son intermill (Misma sociedad con documento 01 Compras) [TYP = G01]
        Y no exista en la misma rama un 04 (Post-Proceso) de la misma sociedad que la a orden de producción) [TYP = G01]
        También elimina los documentos de producción correspondientes a filiales complejas (IS_88_OF_COMPLEX_BRANCHES_FN)

        Parámetros de entrada/Salida:
                  ODS_BUSN_TRX_TREE_TABLE_TYPE

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        29/01/2015      Romero Alejandro -      Creacion del procedure
        24/08/2016      Romero Alejandro -      Se agregque la intermil no tenga un postproceso
*********************************************************************************************/
PROCEDURE get_clean_boughs_sp (
          p_busn_trx_tree_table    IN OUT NOCOPY ODS_BUSN_TRX_TREE_TABLE_TYPE
        )
IS
        v_leaf_of_tree_result_table           ODS_BUSN_TRX_TREE_TABLE_TYPE:= ODS_BUSN_TRX_TREE_TABLE_TYPE();

BEGIN
     BEGIN
        SELECT ODS_BUSN_TRX_TREE_ROW_TYPE(
               BT2.IS_LEAF
              ,BT2.LEAF_PATH_DESC
              ,BT2.PATH_DESC
              ,BT2.ORIGINAL_BUSINESS_ID
              ,BT2.ORIGINAL_BUSINESS_SEQUENCE
              ,BT2.BUSINESS_ID
              ,BT2.BUSINESS_SEQUENCE
              ,BT2.BUSINESS_REFERENCE_ID
              ,BT2.BUSINESS_REFERENCE_SEQ
              ,BT2.DOC_COMM_LOCATION_CODE
              ,BT2.DOCUMENT_YEAR
              ,BT2.DOCUMENT_MONTH
              ,BT2.DOC_TYPE_CODE
              ,BT2.DOC_PORTFOLIO_TYPE
              ,BT2.DOCUMENT_NUMBER
              ,BT2.DOCUMENT_ITEM_NUM
              ,BT2.DOCUMENT_SPLIT_NUM
              ,BT2.FAMILY_TYPE_REF   ) BULK COLLECT INTO v_leaf_of_tree_result_table
        FROM  (
                      /*Obtengo de las ramas todos los documentos que no son Producción ya que no se
                      realiza limpieza de estos documentos*/
                      SELECT BT.*
                      FROM    TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT
                      WHERE   1=1
                      AND     BT.DOC_PORTFOLIO_TYPE <>'88'

                      UNION ALL

                      /*Obtengo de las ramas todos los documentos que son Producción y se realiza
                      limpieza de estos*/
                      SELECT BT.*
                      FROM    TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT
                      WHERE   1=1
                      AND     BT.DOC_PORTFOLIO_TYPE = '88'
                      --No sean Branch complejas
                      AND     IS_88_OF_COMPLEX_BRANCHES_FN( BT.DOC_PORTFOLIO_TYPE
                                                           ,BT.DOC_COMM_LOCATION_CODE) = ODS_COMMON_PKG.C_STR_FALSE
                      --No sean extornos
                      AND     IS_EXTORNOS_FN( BT.BUSINESS_ID
                                             ,BT.DOC_COMM_LOCATION_CODE
                                             ,BT.DOCUMENT_YEAR
                                             ,BT.DOCUMENT_MONTH
                                             ,BT.DOC_TYPE_CODE
                                             ,BT.DOC_PORTFOLIO_TYPE
                                             ,BT.DOCUMENT_NUMBER
                                             ,BT.DOCUMENT_ITEM_NUM
                                             ,BT.DOCUMENT_SPLIT_NUM) = ODS_COMMON_PKG.C_STR_FALSE
                     /*No sea Intermill (Para la misma rama no exista un documento 01 (Compra) de la misma sociedad que la orden de producción) [TYP = G01]
                     Y no exista en esta rama un 04 (Post-Proceso) de la misma sociedad que la a orden de producción) [TYP = G01]*/
                     AND NOT EXISTS(
                                    SELECT 1
		                                FROM   TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT1
		                                WHERE  1=1
		                                AND    BT1.LEAF_PATH_DESC       = BT.LEAF_PATH_DESC
                                    AND    BT1.FAMILY_TYPE_REF      = BT.FAMILY_TYPE_REF
                                    AND    BT1.DOC_PORTFOLIO_TYPE   = '01'
		                                AND    REPLACE (BT1.DOC_COMM_LOCATION_CODE, 'TYP', 'G01') = REPLACE (BT.DOC_COMM_LOCATION_CODE, 'TYP', 'G01')
                                    /*ARO 24-08-2016*/
                                    AND NOT EXISTS(
                                            SELECT 1
		                                        FROM   TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT2
		                                        WHERE  1=1
		                                        AND    BT2.LEAF_PATH_DESC       = BT1.LEAF_PATH_DESC
                                            AND    BT2.FAMILY_TYPE_REF      = BT1.FAMILY_TYPE_REF
                                            AND    BT2.DOC_PORTFOLIO_TYPE   = '04'
		                                        AND    REPLACE (BT2.DOC_COMM_LOCATION_CODE, 'TYP', 'G01') = REPLACE (BT1.DOC_COMM_LOCATION_CODE, 'TYP', 'G01')
                                            )
                                    /*ARO 24-08-2016*/
                                    )
              )BT2;

     EXCEPTION
              WHEN NO_DATA_FOUND THEN
              p_busn_trx_tree_table.DELETE;

              WHEN OTHERS THEN
              p_busn_trx_tree_table.DELETE;
     END;

     p_busn_trx_tree_table.DELETE;--Limpio la variable de salida.
     p_busn_trx_tree_table:=v_leaf_of_tree_result_table;--A la variable de Salida le asigno el valor de la variable calculada
     v_leaf_of_tree_result_table.DELETE;--Limpio la variable calculada
END get_clean_boughs_sp;
--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   GET_BETTER_BOUGH_SP
        Sistema: ODS Business Transaction Utilities
        Objetivo: Pasando el conjunto de cadenas intervinientes de un BID donde ya están armadas las ramas de la cadena.
        y se eliminaron los documentos que no son válidos (GET_CLEAN_BOUGH_SP)
        Se agrupa la información  por rama (Path DESC) Sumraizando los documentos de producción
        y las órdenes de venta.
        Se obtiene la rama que tenga la menor cantidad de documentos de producción (ya que por rama y familia solo tendría que haber un solo
        documento de producción, si hay más es que está mal armada la cadena.) y como segunda prioridad la rama
        que tenga la mayor cantidad de órdenes de venta (que son las que se usan para costear)
        Y desempata por el mayor número de BID.

        Parámetros de entrada/Salida:
            ODS_BUSN_TRX_TREE_TABLE_TYPE

        Notas:
        Autor: Romero Alejandro - S15380
        Historia:
        Fecha           Autor                   Descripción
        29/01/2015      Romero Alejandro -      Creacion del procedure
*********************************************************************************************/
PROCEDURE get_better_bough_sp (
          p_busn_trx_tree_table        IN OUT NOCOPY ODS_BUSN_TRX_TREE_TABLE_TYPE
          )
IS
        v_leaf_path_desc                VARCHAR2(4000);
        v_leaf_of_tree_result_table     ODS_BUSN_TRX_TREE_TABLE_TYPE:= ODS_BUSN_TRX_TREE_TABLE_TYPE();
BEGIN
   BEGIN
      /*Obtengo la rama más significativa según menor cantidad de documentos de producción,
      mayor órdenes de venta y mayor bid*/
      SELECT BT1.LEAF_PATH_DESC INTO v_leaf_path_desc
      FROM (
            SELECT
                   SUM(DECODE (BT.DOC_PORTFOLIO_TYPE,'88',1,NULL)) AS COUNT_MILL
                  ,SUM(DECODE (BT.DOC_PORTFOLIO_TYPE,'02',1,0))    AS COUNT_SO
                  ,MIN(BT.BUSINESS_ID)                             AS BUSINESS_ID
                  ,BT.LEAF_PATH_DESC
            FROM
                  TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT
            GROUP BY
                  BT.LEAF_PATH_DESC
            ORDER BY
                  COUNT_MILL     ASC
                 ,COUNT_SO       DESC
                 ,BUSINESS_ID    DESC
      )BT1
      WHERE ROWNUM =1;
      ----
      /*devuelvo la rama seleccionada en el paso anterior en variable*/
      SELECT ODS_BUSN_TRX_TREE_ROW_TYPE(
               BT.IS_LEAF
              ,BT.LEAF_PATH_DESC
              ,BT.PATH_DESC
              ,BT.ORIGINAL_BUSINESS_ID
              ,BT.ORIGINAL_BUSINESS_SEQUENCE
              ,BT.BUSINESS_ID
              ,BT.BUSINESS_SEQUENCE
              ,BT.BUSINESS_REFERENCE_ID
              ,BT.BUSINESS_REFERENCE_SEQ
              ,BT.DOC_COMM_LOCATION_CODE
              ,BT.DOCUMENT_YEAR
              ,BT.DOCUMENT_MONTH
              ,BT.DOC_TYPE_CODE
              ,BT.DOC_PORTFOLIO_TYPE
              ,BT.DOCUMENT_NUMBER
              ,BT.DOCUMENT_ITEM_NUM
              ,BT.DOCUMENT_SPLIT_NUM
              ,BT.FAMILY_TYPE_REF   ) BULK COLLECT INTO v_leaf_of_tree_result_table
        FROM
                TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT
        WHERE   1=1
        AND     BT.LEAF_PATH_DESC  = v_leaf_path_desc;

   EXCEPTION
   WHEN NO_DATA_FOUND THEN
   p_busn_trx_tree_table.DELETE;

   WHEN OTHERS THEN
   p_busn_trx_tree_table.DELETE;
   END;

   p_busn_trx_tree_table.DELETE;--Limpio la variable de salida.
   p_busn_trx_tree_table:= v_leaf_of_tree_result_table;--A la variable de Salida le asigno el valor de la variable calculada
   v_leaf_of_tree_result_table.DELETE;--Limpio la variable calculada
END get_better_bough_sp;
-----------------------------Function tipo SOBRECARGA de SP --------------------------------------------------------
FUNCTION get_better_bough_fn (
        p_business_id    IN ods_business_transactions.business_id%TYPE
        )
RETURN ODS_BUSN_TRX_TREE_TABLE_TYPE
IS
       /*Cursor para obtener los tipos de productos que tiene el BID*/
       CURSOR cur_family_type_ref
       IS
       SELECT DISTINCT  F.FAMILY_TYPE_REF
       FROM
              ADAS.ODS_BUSINESS_TRANSACTIONS BT
             ,ADAS.ODS_BSN_DOC_TREE_FAMILY_RANK F
       WHERE 1=1
       AND   F.FAMILY_CODE  = NVL(BT.DOCUMENT_FAMILY_CODE,' ')
       AND   BT.BUSINESS_ID = p_business_id;
       ---

       v_cur_family_type_ref_rec        cur_family_type_ref%ROWTYPE;
       v_busn_trx_tree_result_table     ODS_BUSN_TRX_TREE_TABLE_TYPE:= ODS_BUSN_TRX_TREE_TABLE_TYPE();
       v_busn_trx_tree_final_table      ODS_BUSN_TRX_TREE_TABLE_TYPE:= ODS_BUSN_TRX_TREE_TABLE_TYPE();

       v_cant_leaf  PLS_INTEGER;
       v_cant_reg   PLS_INTEGER;

       v_send_mail  BOOLEAN;
       v_time_begin DATE;
BEGIN
  v_send_mail:= FALSE;

  /*Recorro el cursor que indica los distintos tipos de producto que tiene el BID pasado por parámetro*/
  FOR v_cur_family_type_ref_rec IN cur_family_type_ref
  LOOP
       v_time_begin:= SYSDATE;

       --Obtengo recursivamente todos los bid intervinientes en el negocio del bid pasado por parámetro.
       GET_FULL_BUSN_TRX_TREE_SP(
                         /*IN*/  p_business_id                => p_business_id
                         /*IN*/ ,p_family_type_ref            => v_cur_family_type_ref_rec.FAMILY_TYPE_REF
                     /*IN OUT*/ ,p_busn_trx_tree_result_table => v_busn_trx_tree_result_table
                                 );

       --Obtengo la cantidad de registros de la búsqueda recursiva.
       v_cant_reg:=  v_busn_trx_tree_result_table.COUNT;

       IF v_cant_reg >0 THEN--Si obtuve registros
              --Obtengo, marcado con una 'Y' en el campo IS_LEAF si el registro corresponde a una hoja de la rama.
              --También obtengo en la variable v_cant_leaf la cantidad de hojas que se encontraron para el BID.

               GET_LEAF_OF_BUSN_TRX_TREE_SP(
                                /*IN OUT*/  p_busn_trx_tree_table => v_busn_trx_tree_result_table
                                /*IN OUT*/ ,p_count_leaf          => v_cant_leaf
                                           );

               IF v_cant_leaf = 0 OR v_cant_leaf > 300 OR v_cant_reg > 300000 THEN
                   /*Si no se obtuvieron hojas (v_cant_leaf = 0)(mal armada la BT)
                   o la cantidad de registros o Hojas son demasiadas
                   (se calculó que 300 hojas 0 300000 registros para todas las ramas es un valor excesivo)
                   Se toma el BUSINESS_REFERENCE_ID como información de las ramas.
                   Es como se calculaba anteriormente.
                   Hay que tener en cuenta que, de esta manera, puede tener problemas en la limpieza
                   posterior de extornos ya que no se estará pasando el BID correcto.
                   Se marca variable para alertar de este bid por mail.*/
                   GET_LEAFLESS_BOUGH_SP(
                             /*IN OUT*/  p_busn_trx_tree_table => v_busn_trx_tree_result_table
                                         );
                   /*Si entro por acá y no es porque no tenga hojas, es porque
                   la cantidad de hojas o registros es elevado entonces se va a alertar por mail*/
                   IF v_cant_leaf <> 0 THEN
                      v_send_mail:= TRUE;
                   END IF;
               ELSE
                   /*Si hay Hojas y las cantidades no son elevadas Arma las ramas en función de las hojas*/
                   GET_BOUGH_VIA_LEAF_SP(
                             /*IN OUT*/  p_busn_trx_tree_table => v_busn_trx_tree_result_table
                                         );
               END IF;

               /*Realizo la limpieza de los documentos 88*/
               GET_CLEAN_BOUGHS_SP(
                       /*IN OUT*/  p_busn_trx_tree_table => v_busn_trx_tree_result_table
                                   );

               /*Selecciono la mejor rama*/
               GET_BETTER_BOUGH_SP(
                       /*IN OUT*/  p_busn_trx_tree_table => v_busn_trx_tree_result_table
                                   );
               /*Asigno el resultado de la mejor arma para el tipo de producto a la
               variable final, donde acumula las mejores ramas para los tipos de productos*/
               ADD_DATA_SP(v_busn_trx_tree_result_table,v_busn_trx_tree_final_table);

               /*Limpio la variable de trabajo*/
               v_busn_trx_tree_result_table.DELETE;

               IF v_send_mail THEN--Si envia mail
                  v_send_mail:= FALSE;
                  ADAS.UTIL_PKG.SEND_MAIL(
                                          from_name    => 'SUPPORTODSHELPDESK@SIDERCA.COM'
                                         ,to_name      => 'SUPPORTODSHELPDESK@SIDERCA.COM'
                                       --,cc_name      => v_user_mail_cc
                                         ,subject      => 'Selección  de cadena Hevy demasiado grande. BID:' || p_business_id
                                         ,message      => 'ODS_BUSN_TRX_UTILS_PKG.GET_BETTER_BOUGH_FN ' || CHR (13) ||
                                                          'El BID ' || p_business_id || ' tiene ' || v_cant_leaf || ' hojas y recoelcta ' || v_cant_reg || ' registros en su búsqueda recursiva.' || CHR (13) ||
                                                          'El tipo de producto es: ' || v_cur_family_type_ref_rec.FAMILY_TYPE_REF || '.'  || CHR (13) ||
                                                          'Fecha y hora en la que inicio: ' || TO_CHAR (v_time_begin, 'DD/MM/YYYY HH24:MI:SS') || CHR (13) ||
                                                          'Fecha y hora en la que finalizo: ' || TO_CHAR (SYSDATE, 'DD/MM/YYYY HH24:MI:SS')
                                         ,content_type => 'html'
                                         );
               END IF;--End Si envía mail
       END IF;--End Si obtuve registros
  END LOOP;
/*Retorno la variable que acumulo las mejores ramas por diferentes tipo de productos sí es que existen*/
RETURN v_busn_trx_tree_final_table;
END get_better_bough_fn;
-------------------------------------
END ods_busn_trx_utils_pkg;
/
