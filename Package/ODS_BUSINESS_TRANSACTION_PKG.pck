CREATE OR REPLACE PACKAGE ODS_BUSINESS_TRANSACTION_PKG
IS
        /*********************************************************************************************
        Author : Fidel Nakashima (T52585)
        Created : 2010-09-20
        Purpose : Administrar los servicios que permitan acceder e interactuar con Business Transaction
        en ambiente de ODS

        Historial
        Date            Person  Description
        ------------ -----------   -------------------------------------
        2010-09-20 T52585 Creacion del Paquete
        2013-09-26 T32732 Ticket ITTEN00263879
                      Se modificó el SP reclassify_list_busn_trx_sp.
                      Para las cadenas con COMPLETENESS_FLAG = `N¿ y .BROKEN_FLAG  = `N¿ ,
                      hasta los 10 días de su creación, se les volverá a correr el proceso de evaluación.
                      Llegado el dia 10, si  aun SE VERIFICA COMPLETENESS_FLAG = `'N'y .BROKEN_FLAG  = `'N',
                      entonces se marcara el BROKEN FLAG = 'Y' ( DE ESTA MANERA YA NO SERÁ EVALUADA MAS).

        2013-10-02 T32049 ITTEN00264345
        2013-10-03 T32732 Ticket ITTEN00264349
                      Se agregó la función get_busn_trx_created_date_fn
                      ( p_busn_trx_data_table IN ods_busn_trx_table_type)
                      como modificación al criterio para considerar la fecha
                      de inicio del negocio como "in loading process".
                      Se incluye la llamada a la función dentro del SP classify_business_trx_sp.

        2013-10-17 T32732 Ticket ITTEN00266659
                      Se mdifico el SP calc_stutus_class_sp para evitar que el proceso de Clasificación
                      cancele por un ora-error, y deje Bids sin procesar.
                      Se agrego el bloque de EXCEPTION dentro de los SPs classify_business_trx_sp y
                      calc_stutus_class_sp, para el log de errores. Para el registro de errores
                      se creo la tabla ODS_ERROR_LOG_TABLE, y el package ODS_ERROR_LOG_PKG,
                      que inserta dentro de la misma.

        2013-11-06 T32732  Ticket ITTEN00269101
                      Se agrego en el SP insert_busn_class_sp, las función de NVL sobre las columnas:
                      NVL(p_finishing_flag,'N')
                      NVL(p_conforming_flag,'N')

        2014-01-17 T32049  Ticket ITTEN00275682
                      Se agrego el campo ODS_BUSN_TREE_CLASSIFCATION.HISTORICAL_CHAIN_FLAG
                      y se determino su valor dependiendo si la cadena es historica o no.
                      Ademas se creo la funcion set_historical_chain_flag_fn para determinar la logica antes descripta.
                      Se modificaron los SP get_busn_typology_fn, calc_stutus_class_sp, calc_finishing_order_sp
                      calc_conforming_order_sp y insert_busn_class_sp, agregandole un parametro para la nueva
                      logica.

        2014-01-30 T32732  Ticket ITTEN00278399
                                            Se agrego el sp process_build_to_order_trx_sp, el Log de Errores en el bloque de Exception,
                        para registrar los datos del Business_Id que no pueda procesarse.

        2014-05-13 T32732  Ticket ITTEN00278399
                                            GENERATE_DOC_BT_SERV_SALE_SP
                        Una vez que se encontró la cadena, se busca dentro del mismo el primer nodo que
                                sea portfolio 04 (post proceso). Se comenta la condición que hacía coincidir la
                        Commercial Location, para evitar que se genere la cadena sin el Nodo de Servicio.

        2014-05-13 T32732  Ticket ITTEN00302882
                                            Se corrige en el SP calc_stutus_class_sp, los queries dinámicos query3 y query9, ya que
                        la variable v_business_sequence_01 no estaba pasada como parámetro.

        2014-12-10 T32732  Ticket ITTEN00312904
                                       Se agregan los SP's generate_g17_conf_order_sp y generate_doc_g17_conf_order_sp, como parte del requerimeinto
                       de completar cadenas con origen G17 - METALMECANICA. Esto se debe a la integración que hoy existe entre TEN
                       y el sistema Legacy, donde en la mayoría de los casos la orden del TEN queda desvinculada de la orden del
                       Legacy (Conforming Order).
                       La invocación se realiza como último paso del SP generate_busn_trx_sp.

        2015-01-05 T32732  Ticket ITTEN00315133
                                       classify_business_trx_sp
                       Se agrega el SP calc_finishing_plant_sp, para obtener el código de la Planta en donde se lleva a cabo el proceso
                       de Finishing.
                       insert_busn_class_sp
                       Se agrega el parámetro p_finishing_plant_code, para insertar el código de la Planta de Finishing en la
                       tabla ods_busn_tree_classifications .

        2015-04-09 T32732        Se modifico la lógica del SP calc_finishing_plant_sp.

        2015-04-13 T32732        generate_busn_trx_sp
                                            Se agregan los sp's generate_bt_link_extornos_sp y generate_doc_link_extornos_sp, para completar aquellas
                        cadenas con Extornos.
        ----------------------------------------------------------------------------------------------------------------------------------------------
        2015-07-31      T53605 (Pedro Dias)     Ver Documentacion de CALC_FINISHING_ORDER_SP y CALC_FINISHING_PLANT_SP
                                                Modificacion de los SP's que generan clasificacion de negocios en la Busn Tree Classification, se modifica la logica
                                                para el prooceso de generacion de BTC al momento de determinar una Finishing Order para el negocio.
                                                Se identan y nomenclan los SP's para que sean mas legibles en la lectura.
        ----------------------------------------------------------------------------------------------------------------------------------------------
        2015-08-19      T53605 (Pedro Dias)     Ver documentacion de GENERATE_DOC_LINK_EXTORNOS_SP y GENERATE_BT_LINK_EXTORNOS_SP
                                                Modificacion de los SP's que generan los Extornos en la BT, se modifica la logica para el prooceso de generacion
                                                de BT cuando existen Extornos que impactan el procesos de costeo (Inventory Cost).
                                                Se cambia la firma del GENERATE_DOC_LINK_EXTORNOS_SP, elimina un parametros y renombra los existente para legibilidad.
                                                Se identan y nomenclan los SP's para que sean mas legibles en la lectura.
        ----------------------------------------------------------------------------------------------------------------------------------------------
        2015-09-24      T53605 (Pedro Dias)     Ver documentacion de PROCESS_BUILD_TO_ORDER_TRX_SP
                                                Modificacion del SP que genera los enganches de los nodos de la Document Tree en la regeneracion de las cadenas de BT
                                                Los cambios aplicados son:
                                                        1) Se agrega mas detalle al logueo en la cancelacion del proceso cuando existen nodos erroneos en la Doc Tree
                                                        2) Se corrigió el issue para evitar la cancelacion de valores erroneo de las confermas
                                                        3) Se corrigió el issue para evitar la cancelacion en negocios mixtos donde el link Stock (tipo S) no puede
                                                           engancharse a la parte BTO.
                                                        4) Se agrega al logueo los BID que tienen valores erroneos para que sean detectados en algun DQM con mayor facilidad.
        ----------------------------------------------------------------------------------------------------------------------------------------------
        2015-10-19      T53605 (Pedro Dias)     Modificacion proceso de generación de ODS BT para completar campo familia:
                                                Los cambios aplicados son:
                                                        1) generate_bt_serv_sale_sp
	                                                   Agregar columna de familia al cursor cur_bt_serv_sales que apunta a la tabla ODS_BT_SERVICE_SALES
	                                                   Modificar el TYPE ODS_BUSINESS_TRANSACTION_PKG.t_bt_service_sale_rec
                                                        2) generate_doc_bt_serv_sale_sp
	                                                   Agregar la familia a la variable del record type que luego se inserta en la tabla BT, la familia proviene de ODS_BT_SERVICE_SALES
                                                        3) generate_doc_g17_conf_order_sp
	                                                   Agregar la familia a la variable del record type que luego se inserta en la tabla BT
                                                        4) Tomar el dato de la familia correspondiente al padre en caso que el hijo no lo tenga informado, se
                                                           aplica el cambio en los siguientes modulos:
                                                           	1-process_build_to_order_trx_sp
                                                                2-add_pure_build_stk_busn_trx_sp
                                                                3-add_busn_trx_ack_record_sp
                                                                4-add_busn_trx_add_ack_record_sp
                                                                5-add_busn_trx_child_record_sp
                                                                6-add_build_stk_busn_trx_seq_sp
        ----------------------------------------------------------------------------------------------------------------------------------------------
        2015-11-10      T53605 (Pedro Dias)     Modificacion proceso actualizacion de Cadenas de SPIJ, Arabia y Kazahastan:
                                                Los cambios aplicados son:
                                                        1) update_spij_business_sp
                                                           Se incluyeron las sociedades de Arabia y Kazahastan (TSA-KPT) al cursor que determina los BID a procesar
                                                        2) update_doc_spij_business_sp
                                                           Se incluyeron las sociedades de Arabia y Kazahastan (TSA-KPT) al proceso de actualizacion del BID
        ----------------------------------------------------------------------------------------------------------------------------------------------
        2015-12-21       (Alejandro Romero)     Se genera un nuevo SP para la completitud del codigo de familia para cada secuencia del negocio:
                                                COMPLETENESS_FAMILY_PRODUCT_SP
        ----------------------------------------------------------------------------------------------------------------------------------------------
        2016-03-03      (Alejandro Romero)     1.-Se mejora el SP COMPLETENESS_FAMILY_PRODUCT_SP, para que solo updete si hay una nueva familia.
                                               2.-Se crea un nuevo SP (update_complex_branches_bus_sp) para marcar la SECONDARY_COSTING_FLAG si las branch's son complejas y son documentos de produccion (88) usando la ODS_BUSN_TRX_UTILS_PKG.IS_88_OF_COMPLEX_BRANCHES_FN
                                               3.-Se comentan los SP update_spij_business_sp-->update_doc_spij_business_sp ya que son remplazados por el update_complex_branches_bus_sp del punto 2
                                               4.---Se coloca la nueva funcion update_complex_branches_bus_sp luego de la generacion de los links de extornos (generate_bt_link_extornos_sp)
        ----------------------------------------------------------------------------------------------------------------------------------------------
        2016-05-10     T32068 (Cristian Ayala) 1.-Se mejora el SP get_fam_busn_trx_child_nodes, modificando la condición de marcado de rama de mayor ranking, antes se hacía por secuencia y ahora marca a todas las ramas como primarias que contengan a la misma familia
                        ITTEN00367516          2.-Se mejora el SP delete_error_busn_id_sp, en el cual se estaban borrando cadenas de la work que no encontraron su cadena de referencia y por lo tanto no se clasificaban y no subian al DWT.
        ----------------------------------------------------------------------------------------------------------------------------------------------
        2016-06-30     T32068 (Cristian Ayala) 1.-Se mejora el SP COMPLETENESS_FAMILY_PRODUCT_SP, para que solo actualice cuando se encuentra la orden (en los modelos comerciales) y la familia es nula en la business transactions.
                        ITTEN00371400          2.-Se agrega un union al SP classify_list_business_trx_sp para re-clasificar negocios incompletos de ordenes de compra de tercero  que llegaron luego del ultimo update de la clasificacion.
        ---------------------------------------------------------------------------------------------------------------------------------------------------------
        2016-08-17     T32068 (Cristian Ayala) 1.-Se agrega un insert en la tabla de trabajo al SP classify_list_business_trx_sp para re-clasificar negocios incompletos de ordenes de compra de tercero  que llegaron luego del ultimo update de la clasificacion.
                        ITTEN00374517
        -----------------------------------------------------------------------------------------------------------------------------------------
        2016-08-17     T32068 (Cristian Ayala) 1.- Incidencia en el calc_stutus_class_sp, en el mismo se generaba este error ORA-00904: "ODS_BUSINESS_TRANSACTION_PKG"."Ñ": identificador no válido.
                        ITTEN00382558
        -----------------------------------------------------------------------------------------------------------------------------------------

        *********************************************************************************************/

        -- Public type declarations
        TYPE t_refcursor IS REF CURSOR;

        TYPE t_busn_trx_tab_row IS RECORD
        (
                business_id                     ods_business_transactions.business_id%TYPE,
                business_sequence               ods_business_transactions.business_sequence%TYPE,
                business_reference_id           ods_business_transactions.business_reference_id%TYPE,
                business_reference_seq          ods_business_transactions.business_reference_seq%TYPE,
                doc_portfolio_type              ods_business_transactions.doc_portfolio_type%TYPE,
                doc_comm_location_code          ods_business_transactions.doc_comm_location_code%TYPE,
                document_year                   ods_business_transactions.document_year%TYPE,
                document_month                  ods_business_transactions.document_month%TYPE,
                doc_type_code                   ods_business_transactions.doc_type_code%TYPE,
                document_number                 ods_business_transactions.document_number%TYPE,
                document_item_num               ods_business_transactions.document_item_num%TYPE,
                document_split_num              ods_business_transactions.document_split_num%TYPE,
                unlinked_doc_flag               ods_business_transactions.unlinked_doc_flag%TYPE,
                data_source_system              ods_business_transactions.data_source_system%TYPE,
                parent_doc_comm_loc             ods_business_transactions.parent_doc_comm_loc%TYPE,
                parent_doc_portfolio_type       ods_business_transactions.parent_doc_portfolio_type%TYPE,
                parent_doc_year                 ods_business_transactions.parent_doc_year%TYPE,
                parent_doc_month                ods_business_transactions.parent_doc_month%TYPE,
                parent_doc_type_code            ods_business_transactions.parent_doc_type_code%TYPE,
                parent_doc_number               ods_business_transactions.parent_doc_number%TYPE,
                parent_doc_item_num             ods_business_transactions.parent_doc_item_num%TYPE,
                parent_doc_split_num            ods_business_transactions.parent_doc_split_num%TYPE,
                historical_link_flag            ods_business_transactions.historical_link_flag%TYPE,
                secondary_costing_flag          ods_business_transactions.secondary_costing_flag%TYPE,
                origin_business_id              ods_business_transactions.origin_business_id%TYPE,
                document_family_code            ods_business_transactions.document_family_code%TYPE,
                manual_record_flag              ods_common_pkg.s_str_yes_no,
                integration_type                ods_business_transactions.integration_type%TYPE,
                created_date                    ods_business_transactions.created_date%TYPE,
                created_by                      ods_business_transactions.created_by%TYPE,
                last_updated_date               ods_business_transactions.last_updated_date%TYPE,
                last_updated_by                 ods_business_transactions.last_updated_by%TYPE,
                deleted_date                    ods_business_transactions.deleted_date%TYPE,
                deleted_by                      ods_business_transactions.deleted_by%TYPE
        );

        TYPE t_business_trx_id IS RECORD
        (
                business_id             ods_business_transactions.business_id%TYPE,
                business_sequence       ods_business_transactions.business_sequence%TYPE
        );

        TYPE t_bt_service_sale_rec IS RECORD
        (
                ss_comm_loc_code        ods_bt_service_sales.ss_comm_loc_code%TYPE,
                ss_order_year           ods_bt_service_sales.ss_order_year%TYPE,
                ss_order_month          ods_bt_service_sales.ss_order_month%TYPE,
                ss_doc_type_code        ods_bt_service_sales.ss_doc_type_code%TYPE,
                ss_number               ods_bt_service_sales.ss_number%TYPE,
                ss_item_num             ods_bt_service_sales.ss_item_num%TYPE,
                ss_split_num            ods_bt_service_sales.ss_split_num%TYPE,
                --
                ss_family_code          ods_bt_service_sales.ss_family_code%TYPE,
                --
                business_id             ods_bt_service_sales.business_id%TYPE,
                so_comm_loc_code        ods_bt_service_sales.so_comm_loc_code%TYPE,
                so_order_year           ods_bt_service_sales.so_order_year%TYPE,
                so_order_month          ods_bt_service_sales.so_order_month%TYPE,
                so_doc_type_code        ods_bt_service_sales.so_doc_type_code%TYPE,
                so_number               ods_bt_service_sales.so_number%TYPE,
                so_item_num             ods_bt_service_sales.so_item_num%TYPE,
                so_split_num            ods_bt_service_sales.so_split_num%TYPE
        );


        TYPE t_busn_class_rec IS RECORD
        (
                business_id                     ods_busn_tree_classifications.business_id%TYPE,
                business_typology_code          ods_busn_tree_classifications.business_typology_code%TYPE,
                completeness_flag               ods_busn_tree_classifications.completeness_flag%TYPE,
                broken_flag                     ods_busn_tree_classifications.broken_flag%TYPE,
                finishing_order_key             ods_busn_tree_classifications.finishing_order_key%TYPE,
                conforming_order_key            ods_busn_tree_classifications.conforming_order_key%TYPE,
                finishing_process_classn_code   ods_busn_tree_classifications.finishing_process_classn_code%TYPE,
                tenaris_conforming_mill_flag    ods_busn_tree_classifications.tenaris_conforming_mill_flag%TYPE,
                completed_business_ref_id       ods_busn_tree_classifications.completed_business_ref_id%TYPE,
                data_source_system              ods_busn_tree_classifications.data_source_system%TYPE,
                process_date                    ods_busn_tree_classifications.process_date%TYPE,
                created_by                      ods_busn_tree_classifications.created_by%TYPE,
                created_date                    ods_busn_tree_classifications.created_date%TYPE,
                last_updated_by                 ods_busn_tree_classifications.last_updated_by%TYPE,
                last_updated_date               ods_busn_tree_classifications.last_updated_date%TYPE
        );

        TYPE t_business_trx_id_tab IS TABLE OF t_business_trx_id
        INDEX BY BINARY_INTEGER;

        -- tabla de registro business_transactions
        TYPE t_busn_trx_tab IS TABLE OF t_busn_trx_tab_row
        INDEX BY BINARY_INTEGER;

        -- tabla de registro business_tree_classifications
        TYPE t_busn_class_tab IS TABLE OF t_busn_class_rec
        INDEX BY BINARY_INTEGER;

        -- Public constant declarations

        c_doc_num_length                CONSTANT PLS_INTEGER := 8;
        c_doc_month_length              CONSTANT PLS_INTEGER := 2;

        c_get_busn_trx_ref_process      CONSTANT VARCHAR2 (10) := 'GET_BID_RF';
        c_replace_busn_trx_process      CONSTANT VARCHAR2 (10) := 'REP_BSNTRX';
        c_ref_busn_trx_regen_process    CONSTANT VARCHAR2 (10) := 'REF_BSNTRX';
        c_stk_branch_busn_trx_process   CONSTANT VARCHAR2 (10) := 'STK_BRANCH';
        c_secondary_cost_process        CONSTANT VARCHAR2 (10) := 'SECON_COST';
        c_bt_serv_sale_process          CONSTANT VARCHAR2 (10) := 'BT_SERVSAL';
        c_busn_tree_classn              CONSTANT VARCHAR2 (10) := 'BSN_CLASSN';
        c_busn_tree_rep_classn          CONSTANT VARCHAR2 (10) := 'REP_CLASSN';

        c_invalid_busn_trx_ref_id       CONSTANT ods_business_transactions.business_reference_id%TYPE:= 9999999999 ;
        c_invalid_busn_trx_ref_seq      CONSTANT ods_business_transactions.business_reference_seq%TYPE:= 99999999 ;
        c_max_busn_trx_ref_seq          CONSTANT ods_business_transactions.business_reference_seq%TYPE:= 99999999 ;
        c_max_busn_trx_seq              CONSTANT ods_business_transactions.business_sequence%TYPE:= 99999999 ;
        c_invalid_busn_trx_source       CONSTANT ods_business_transactions.data_source_system%TYPE:= 'INVALID_RF' ;

        c_proc_responsible_email        CONSTANT VARCHAR2 (200):= 'mbaigorria@tenaris.com' ;
        --c_proc_responsible_email CONSTANT VARCHAR2(200):= 'sidtnfi@proveedores.siderca.com';


        -- Public variable declarations

        -- Public Exceptions
        e_doc_tree_node_twice_insert    EXCEPTION;
        e_no_data_found_on_regen        EXCEPTION;

        -- Public function and procedure declarations

        ----------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   generate_busn_trx_sp
         Sistema: ODS
         Objetivo: generar / actualiar la tabla ODS_BUSINESS_TRANSACTION a partir de los datos del arbol de
          documentos. Toma como imput los registros que se identifican dentro de la tabla  ODS_BUSINESS_DOC_TREE_WRK

         Parámetros de entrada:

         Parámetros de salida:

         Notas:
         Autor: T52585
         Historia:
         Fecha Autor Descripción
         2010-09-20   T52585  Creacion de procedimiento

        *********************************************************************************************/
        PROCEDURE generate_busn_trx_sp;


        ----------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   is_build_to_order_busn_str_fn
         Sistema: ODS
         Objetivo: verifica si una cadena de documentos es "build to order" (back to back).
          Para ello dado un business_id verifica si todos los nodos del arbol de documento
          que pertenecen al arbol poseen tipos de link <> "S"(Stock)

         Parámetros de entrada:
         p_business_id: identificador de business del arbol de documentos

         Parámetros de salida:
         salida de funcion: retorna un string 'TRUE' en caso de tratarse de un arbol "Build to Order",
          'FALSE' en caso contrario

         Notas:
         Autor: T52585
         Historia:
         Fecha Autor Descripción
         2010-09-22   t52585  Creacion de modulo

        *********************************************************************************************/

        FUNCTION is_build_to_order_busn_str_fn (
                p_business_id IN ods_business_transactions.business_id%TYPE)
                RETURN VARCHAR2;

        ----------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   is_busn_trx_ref_no_found_fn
         Sistema: ODS
         Objetivo: verifica si el mensaje de error corresponde a una cadena incompleta
          Para ello se compara el mensaje enviado con la constante c_busn_trx_ref_no_found_desc.
          NOTA: se usan las descripciones porque no se generan codigos de error para estas excepciones.

         Parámetros de entrada:
         p_message_desc: descripcion del mensaje de error

         Parámetros de salida:
         salida de funcion: TRUE si corresponde a una cadena incompleta,
         FALSE en caso contrario

         Notas:
         Autor: T31105
         Historia:
         Fecha Autor Descripción
         2011-01-19   t31105  Creacion de modulo

        *********************************************************************************************/


        FUNCTION is_busn_trx_ref_no_found_fn (
                p_message_desc IN ods_busn_doc_tree_error_pkg.s_error_desc)
                RETURN BOOLEAN;


        ---------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   is_build_stock_doc_tree_str_fn
         Sistema: ODS
         Objetivo: verifica si una cadena de documentos es "puro stock"

         Parámetros de entrada:
         p_business_id: identificador de business del arbol de documentos

         Parámetros de salida:
         salida de funcion: retorna un string 'TRUE' en caso de tratarse de un arbol "Puro Stock",
          'FALSE' en caso contrario

         Notas:
         Autor: T52585
         Historia:
         Fecha Autor Descripción
         2011-08-16   t52585  Creacion de modulo

        *********************************************************************************************/
        FUNCTION is_build_stock_doc_tree_str_fn (
                p_business_id ods_business_doc_tree_wrk.business_id%TYPE)
                RETURN VARCHAR2;

        ---------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   get_business_transaction_sp
         Sistema: ODS
         Objetivo: dado un BUSINESS_ID recupera la cadena de Business Transaction correspondiente. Dependiendo
         si se indica o no en el parametro P_INCLUDE_MANUAL_NODES_FLAG incluye los nodos que estan cargados
         en la tabla ODS_BUSINESS_DOC_TREE_MAN que aun no se han incorporado al BUSINESS TRANSACTION
         correspondiente.

         Parámetros de entrada:
         p_business_id: identificador de business_id del Business Transaction
         p_include_manual_flag: flag booleano en formato string ('T' o 'F') que indica si se incluyen
         en el resultado los nodos manuales recuperados de la tabla ODS_BUSINESS_DOC_TREE_MAN o no

         Parámetros de salida:
         p_cur_busn_trx: ref cursor con registros del tipo de datos t_busn_trx_tab_row

         Notas:
         Autor: T52585
         Historia:
         Fecha Autor Descripción
         2011-12-20  t52585  Creacion de modulo

        *********************************************************************************************/
        PROCEDURE get_business_transaction_sp (
                p_business_id                        IN           ods_business_transactions.business_id%TYPE,
                p_include_manual_flag        IN           ods_common_pkg.s_str_boolean DEFAULT ods_common_pkg.
                                                                                                                                                         c_str_true,
                p_cur_busn_trx                           OUT ods_common_pkg.t_refcursor);


        ---------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   get_business_transaction_sp
         Sistema: ODS
         Objetivo: sobre carga del sp con el mismo nombre get_business_transaction_sp.
         Dados los datos identificatorios de una orden y el tipo de cadena (Build to Order o Build to Stock)
         recupera la cadena completa a la que pertenece la orden especificada.
         En la busqueda de la cadena se verifica que la cadena encontrada cumpla con
         esa condicion Build to Order o Build to Stock segun se haya especiifcado.
         Dependiendo si se indica o no en el parametro P_INCLUDE_MANUAL_NODES_FLAG incluye los nodos que estan cargados
         en la tabla ODS_BUSINESS_DOC_TREE_MAN que aun no se han incorporado al BUSINESS TRANSACTION
         correspondiente.

         Parámetros de entrada:
         p_doc_comm_loc_code: Identificador unívoco de una sociedad Tenaris (dominio TEN)
         p_document_year: Año correspondiente a la fecha de alta del documento comercial
         p_doc_type_code: Tipo de documento comercial (Orden de venta, Orden de compra, etc) del split buscado (dominio TEN)
         p_document_number: Número del documento comercial
         p_document_item_num: Número de item del documento comercial
         p_document_split_num: Número de split del documento comercial
         p_include_manual_flag: flag booleano en formato string ('T' o 'F') que indica si se incluyen
         en el resultado los nodos manuales recuperados de la tabla ODS_BUSINESS_DOC_TREE_MAN o no

         Parámetros de salida:
         p_cur_busn_trx: ref cursor con registros del tipo de datos t_busn_trx_tab_row

         Notas:
         Autor: T52585
         Historia:
         Fecha Autor Descripción
         2011-12-20  t52585  Creacion de modulo

        *********************************************************************************************/
        PROCEDURE get_business_transaction_sp (
                p_doc_comm_loc_code         IN           ods_business_transactions.
                                                                                doc_comm_location_code%TYPE,
                p_document_year                 IN           ods_business_transactions.document_year%TYPE,
                p_document_month                IN           ods_business_transactions.document_month%TYPE,
                p_doc_type_code                 IN           ods_business_transactions.doc_type_code%TYPE,
                p_document_number                IN           ods_business_transactions.
                                                                                document_number%TYPE,
                p_document_item_num         IN           ods_business_transactions.
                                                                                document_item_num%TYPE,
                p_document_split_num        IN           ods_business_transactions.
                                                                                document_split_num%TYPE,
                p_include_manual_flag        IN           ods_common_pkg.s_str_boolean,
                p_chain_type                        IN           ods_common_pkg.s_busn_chain_type,
                p_cur_busn_trx                           OUT ods_common_pkg.t_refcursor);



        ---------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   get_business_transaction_sp
         Sistema: ODS
         Objetivo: sobre carga del sp con el mismo nombre get_business_transaction_sp.
         Esta version verifica la existencia de un valor para el parametro p_business_id, si
          el mismo no es nulo trata de recuperar la cadena Business Transaction por medio del mismo.
          Si no se especifica ningun valor en este parametro trata de recuperar la cadena por medio
          de los datos del documento
         En la busqueda de la cadena se verifica que la cadena encontrada cumpla con
         esa condicion Build to Order o Build to Stock segun se haya especiifcado.
         Dependiendo si se indica o no en el parametro P_INCLUDE_MANUAL_NODES_FLAG incluye los nodos que estan cargados
         en la tabla ODS_BUSINESS_DOC_TREE_MAN que aun no se han incorporado al BUSINESS TRANSACTION
         correspondiente.

         Parámetros de entrada:
         p_business_id: identificador de business_id del Business Transaction
         p_doc_comm_loc_code: Identificador unívoco de una sociedad Tenaris (dominio TEN)
         p_document_year: Año correspondiente a la fecha de alta del documento comercial
         p_doc_type_code: Tipo de documento comercial (Orden de venta, Orden de compra, etc) del split buscado (dominio TEN)
         p_document_number: Número del documento comercial
         p_document_item_num: Número de item del documento comercial
         p_document_split_num: Número de split del documento comercial
         p_include_manual_flag: flag booleano en formato string ('T' o 'F') que indica si se incluyen
         en el resultado los nodos manuales recuperados de la tabla ODS_BUSINESS_DOC_TREE_MAN o no

         Parámetros de salida:
         p_cur_busn_trx: ref cursor con registros del tipo de datos t_busn_trx_tab_row

         Notas:
         Autor: T52585
         Historia:
         Fecha Autor Descripción
         2011-12-27  t52585  Creacion de modulo

        *********************************************************************************************/
        PROCEDURE get_business_transaction_sp (
                p_business_id                        IN           ods_business_transactions.business_id%TYPE,
                p_doc_comm_loc_code         IN           ods_business_transactions.
                                                                                doc_comm_location_code%TYPE,
                p_document_year                 IN           ods_business_transactions.document_year%TYPE,
                p_document_month                IN           ods_business_transactions.document_month%TYPE,
                p_doc_type_code                 IN           ods_business_transactions.doc_type_code%TYPE,
                p_document_number                IN           ods_business_transactions.
                                                                                document_number%TYPE,
                p_document_item_num         IN           ods_business_transactions.
                                                                                document_item_num%TYPE,
                p_document_split_num        IN           ods_business_transactions.
                                                                                document_split_num%TYPE,
                p_include_manual_flag        IN           ods_common_pkg.s_str_boolean,
                p_chain_type                        IN           ods_common_pkg.s_busn_chain_type,
                p_cur_busn_trx                           OUT ods_common_pkg.t_refcursor);

        ----------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   remove_busn_class_by_bid_sp
         Sistema: ODS
         Objetivo: borra los registros de la tabla de ods_busn_tree_classifications en base al
          business_id  que se especifica. Esto puede borrar mas de 1 registro

         Parámetros de entrada:
         p_business_id: identificador de business a borrar

         Parámetros de salida:


         Notas:
         Autor: T53605
         Historia:
         Fecha Autor Descripción
         2013-03-13   T53605  Creacion de modulo

        *********************************************************************************************/
        PROCEDURE remove_busn_class_by_bid_sp (
                p_business_id                 ods_business_transactions.business_id%TYPE,
                p_created_date         OUT ods_busn_tree_classifications.created_date%TYPE,
                p_created_by         OUT ods_busn_tree_classifications.created_by%TYPE);

        ----------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   remove_busn_class_sp
         Sistema: ODS
         Objetivo: borra los registros de la tabla de ods_busn_tree_classifications en base a los
          business_id que se especifican en la ods_business_doc_tree_wrk. Esto puede borrar mas de 1 registro

         Parámetros de entrada:


         Parámetros de salida:


         Notas:
         Autor: T53605
         Historia:
         Fecha Autor Descripción
         2013-03-13  T53605  Creacion de modulo

        *********************************************************************************************/
        PROCEDURE remove_busn_class_sp;

        ---------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   classify_business_trx_sp
         Sistema: ODS
         Objetivo: dado un business_id y el data source system, se clasificara el business transaction con respecto a
         las propiedades :
          - Business Tipology
          - Completeness Status
          - Broken Status
          - Conforming Mill Order
          - Finishing Order
          - Finshing process Type

          Estas caracteristicas se almacenaran en la tabla ODS_BUSN_TREE_CLASSIFICATION

         Parámetros de entrada:
         p_business_id: identificador de business_id del Business Transaction
          p_data_source_system: sistema origen del business transaction

         Parámetros de salida:

         Notas:
         Autor: T52585
         Historia:
         Fecha Autor Descripción
         2013-02-27    t52585  Creacion de modulo
         2013-10-17    T32732  Ticket ITTEN00266659
          Se agrego el bloque de EXCEPTION para el log de errores. Para el registro de errores
          se creo la tabla ODS_ERROR_LOG_TABLE, y el package ODS_ERROR_LOG_PKG,
          que inserta dentro de la misma.

        *********************************************************************************************/
        PROCEDURE classify_business_trx_sp (
                p_business_id                   IN ods_business_transactions.business_id%TYPE,
                p_data_source_system   IN ods_business_transactions.
                                                                   data_source_system%TYPE);



        ---------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   classify_list_business_trx_sp
         Sistema: ODS
         Objetivo: se clasificaran los business transactions especificado en la tabla ODS_BUSINESS_DOC_TREE_WRK.
         Se clasificara el business transaction con respecto a
         las propiedades :
          - Business Tipology
          - Completeness Status
          - Broken Status
          - Conforming Mill Order
          - Finishing Order
          - Finshing process Type

          Estas caracteristicas se almacenaran en la tabla ODS_BUSN_TREE_CLASSIFICATION

         Parámetros de entrada:

         Parámetros de salida:

         Notas:
         Autor: T52585
         Historia:
         Fecha Autor Descripción
         2013-02-27  t52585    Creacion de modulo

        *********************************************************************************************/
        PROCEDURE classify_list_business_trx_sp;

        ---------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   is_supplier_not_tenaris_fn
         Sistema: ODS
         Objetivo: determinar si una orden de compra corresponde a un proveedor NO TENARIS

         Parámetros de entrada:
                p_doc_comm_loc: Codigo de la Document Commercial Location
                p_doc_num: Numero de documento
                p_doc_month: Mes del documento
                p_doc_year: Año del documento
                p_doc_type: Codigo del tipo de documento

         Parámetros de salida:

         Notas:
         Autor: T53605
         Historia:
         Fecha Autor Descripción
         2013-03-21  T53605    Creacion de modulo

        *********************************************************************************************/
        FUNCTION is_supplier_not_tenaris_fn ( --p_busn_trx_data_table  IN ods_busn_trx_table_type
                p_doc_comm_loc  IN ods_business_transactions.doc_comm_location_code%TYPE,
                p_doc_num       IN ods_business_transactions.document_number%TYPE,
                p_doc_month     IN ods_business_transactions.document_month%TYPE,
                p_doc_year      IN ods_business_transactions.document_year%TYPE,
                p_doc_type      IN ods_business_transactions.doc_type_code%TYPE)
        RETURN VARCHAR2;

        FUNCTION exist_delivery_fn (
                p_doc_comm_loc         IN ods_business_transactions.doc_comm_location_code%TYPE,
                p_doc_num                 IN ods_business_transactions.document_number%TYPE,
                p_doc_month          IN ods_business_transactions.document_month%TYPE,
                p_doc_year                 IN ods_business_transactions.document_year%TYPE,
                p_doc_type                 IN ods_business_transactions.doc_type_code%TYPE,
                p_doc_itm                 IN ods_business_transactions.document_item_num%TYPE,
                p_doc_spl                 IN ods_business_transactions.document_split_num%TYPE)
                RETURN VARCHAR2;

        FUNCTION exist_stock_fn (
                p_doc_comm_loc          IN ods_business_transactions.doc_comm_location_code%TYPE,
                p_doc_num                  IN ods_business_transactions.document_number%TYPE,
                p_doc_month           IN ods_business_transactions.document_month%TYPE,
                p_doc_year                  IN ods_business_transactions.document_year%TYPE,
                p_doc_type                  IN ods_business_transactions.doc_type_code%TYPE,
                p_doc_itm                  IN ods_business_transactions.document_item_num%TYPE,
                p_doc_spl                  IN ods_business_transactions.document_split_num%TYPE,
                p_doc_portfolio   IN ods_business_transactions.doc_portfolio_type%TYPE)
                RETURN VARCHAR2;

        FUNCTION set_historical_chain_flag_fn (
                p_busn_trx_data_table   IN ods_busn_trx_table_type)
        RETURN ods_busn_tree_classifications.historical_chain_flag%TYPE;

        FUNCTION get_busn_typology_fn (
                p_busn_trx_data_table   IN ods_busn_trx_table_type)
        RETURN ods_business_typologies.business_typology_code%TYPE;

        PROCEDURE calc_stutus_class_sp (
                p_busn_trx_data_table          IN         ods_busn_trx_table_type,
                p_typology_code                   IN         ods_busn_tree_classifications.
                                                                                  business_typology_code%TYPE,
                p_historical_chain_flag   IN         ods_busn_tree_classifications.
                                                                                  historical_chain_flag%TYPE,
                p_completeness_flag                  OUT ods_busn_tree_classifications.
                                                                                  completeness_flag%TYPE,
                p_broken_flag                                 OUT ods_busn_tree_classifications.
                                                                                  broken_flag%TYPE,
                p_ten_conf_mill_flag                 OUT ods_busn_tree_classifications.
                                                                                  tenaris_conforming_mill_flag%TYPE,
                p_business_ref_id                         OUT ods_busn_tree_classifications.
                                                                                  business_id%TYPE);
        /*********************************************************************************************
        Nombre del programa:   calc_finishing_order_sp
            Sistema: ODS
            Objetivo: Dados los parámetros de entradas, recupera la cadena de documentos, e inserta al
            final de la misma el nodo correspondiente a la Orden de Producción que fabricó los tubos,
            realizando la vinculación con esta otra cadena mediante el BUSINESS_ID_REFERENCE.

            Parámetros de entrada:
                p_busn_trx_data_table
                p_typology_code
                p_historical_chain_flag


            Parámetros de salida:
                p_finishing_order_key
                p_finishing_process_code
                p_finishing_flag

            Notas:
            Autor:
            Historia:
            Fecha        Autor                  Descripción
            ---------------------------------------------------
                                                Creacion de procedimiento
            ---------------------------------------------------
                                                Modificacion de procedimiento
            ---------------------------------------------------
            2015-07-31  T53605 (Pedro Dias)     Identacion y nomenclacion del SP para tener legibilidad en la lectura del codigo.
                                                Por nueva definicion de negocio se cambio el codigo que determina la seleccion de una Finishing Order, Se mantiene
                                                SQL Dinamico pero se mejora la calidad y legibilidad en el armado del query dinammico para la determinacion de la
                                                nueva Finishing Order.
                                                Eliminacion de variables globales que luego utiliza el CALC_FINISHING_PLANT_SP.
                                                Cuando se verifica que exista una orden de compra asociada a la Finishing Order tambien se retorna la orden de compra
                                                para luego determinar si esa orden de compra corresponde a un Cliente NO Tenaris.
                                                Se elimina codigo muerto del procedure.
            ---------------------------------------------------

        *********************************************************************************************/
        PROCEDURE calc_finishing_order_sp (
                        p_busn_trx_data_table           IN ods_busn_trx_table_type,
                        p_typology_code                 IN ods_busn_tree_classifications.business_typology_code%TYPE,
                        p_historical_chain_flag         IN ods_busn_tree_classifications.historical_chain_flag%TYPE,
                        p_finishing_order_key           OUT ods_busn_tree_classifications.finishing_order_key%TYPE,
                        p_finishing_process_code        OUT ods_busn_tree_classifications.finishing_process_classn_code%TYPE,
                        p_finishing_flag                OUT ods_busn_tree_classifications.finishing_reference_flag%TYPE);


        ----------------------------------------------------------------------------------------------
        /*********************************************************************************************
        Nombre del programa:   calc_finishing_plant_sp
         Sistema: ODS
         Objetivo: Obtener la planta que realiza el proceso de Finishing. Primero se busca en la cadena que exista
                     una Orden de Finishing. En ese caso, se busca una Orden de Venta en la cadena, y si se encuentra,
             se busca la planta en la Orden de Venta (tabla ods_bt_service_sales). Esto correspondería al
             modelo Hydril del negocio.
                     Sino se encuentra una Orden de Venta, se busca la planta en la Orden de Producción.
            (tabla ods_production_orders_itm).

         Parámetros de entrada:
                 p_busn_trx_data_table: Parámetro de tipo Colletión Type (ods_busn_trx_table_type), conteniendo la cadena
                                        de documentos correspondiente al Business que se esta procesando.
                 p_finishing_order_key: Key de la Finishing Order obtenida por el CALC_FINISHING_ORDER_SP

         Parámetros de salida:
                 p_finishing_plant: Retorna la Planta de Finishing, para luego insertarla en ODS_BUSINESS_TRANSACTIONS.

         Notas:
         Autor: T32732
         Historia:
         Fecha          Autor           Descripción
         2015-01-05     t32732          Creacion del procedimiento
         ---------------------------------------------------
         2015-08-03     T53605          Cambio de firma, se agrego un nuevo parametro de entrada para evitar el uso de varibles globales en el procedure.
                                        Se eliminan las variables globales generadas para respetar Standard y Best Practices Tenaris. En su reemplazo el SP
                                        recibe como parametro la Finishing Order Key obtenida por el CALC_FINISHING_ORDER_SP y la parsea para poder obtener
                                        el Plant Code de la Orden de Produccion correspondiente.

        *********************************************************************************************/
        PROCEDURE calc_finishing_plant_sp (
                p_busn_trx_data_table   IN ods_busn_trx_table_type,
                p_finishing_order_key   IN ods_busn_tree_classifications.finishing_order_key%TYPE,
                p_finishing_plant       OUT ods_busn_tree_classifications.finishing_plant_code%TYPE);

        PROCEDURE calc_conforming_order_sp (
                p_busn_trx_data_table         IN         ods_busn_trx_table_type,
                p_completeness_flag          IN         ods_busn_tree_classifications.
                                                                                 completeness_flag%TYPE,
                p_historical_chain_flag    IN          ods_busn_tree_classifications.
                                                                                   historical_chain_flag%TYPE,
                p_conforming_order_key                OUT ods_busn_tree_classifications.
                                                                                 finishing_order_key%TYPE,
                p_conforming_flag                        OUT ods_busn_tree_classifications.
                                                                                 conforming_reference_flag%TYPE);

        --------------------------------------------------------------------------------
        PROCEDURE reclassify_list_busn_trx_sp;
--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   completeness_family_product_sp
Sistema: ODS
Objetivo: Completar el codigo de familia de producto para cada secuencia del negocio

Parámetros de entrada:
        p_range_in_days: Parametro que indica la cantidad de dias a los que se le quiere completar la familia. Por default asigna NULL, con lo que se realizaria
                        una completitud de toda la tablas, caso contrario solo un rango de dias.

Parámetros de salida:


Notas:
Autor:
Historia:
Fecha          Autor           Descripción
2015-12-21               Creacion del procedimiento
---------------------------------------------------
*********************************************************************************************/
PROCEDURE completeness_family_product_sp(
        p_range_in_days IN PLS_INTEGER DEFAULT NULL
        );
--------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   update_complex_branches_bus_sp
    Sistema: ODS
    Objetivo: darcar la SECONDARY_COSTING_FLAG si las branch's son complejas y son
    documentos de produccion (88) usando la ODS_BUSN_TRX_UTILS_PKG.IS_88_OF_COMPLEX_BRANCHES_FN

    Notas:
    Autor: 15380 - Romero Alejandro
    Historia:
    Fecha               Autor                           Descripción
    20116-03-03         15380                          Creacion del SP
*********************************************************************************************/
PROCEDURE update_complex_branches_bus_sp;

--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa: completeness_heavy_sp
    Sistema: ODS
    Objetivo: completa el campo Heavy (MAIN_BOUGH_BY_TYPE_FLAG) de la ODS_BUSINESS_TRANSACTIONS
    Segun la cadena mas sigificativa para el tipo de cadenas

    Notas:
    Autor: 15380 - Romero Alejandro
    Historia:
    Fecha               Autor                           Descripción
    20116-03-03         15380                          Creacion del SP
*********************************************************************************************/
PROCEDURE completeness_heavy_sp;

END ODS_BUSINESS_TRANSACTION_PKG;
/
CREATE OR REPLACE PACKAGE BODY ODS_BUSINESS_TRANSACTION_PKG
IS
        -- Private type declarations
        TYPE t_busn_id_rec IS RECORD (
                business_id     ods_business_doc_tree_ref_wrk.business_id%TYPE,
                created_by      ods_business_doc_tree_ref_wrk.created_by%TYPE,
                created_date    ods_business_doc_tree_ref_wrk.created_date%TYPE
                );

        TYPE t_family_code_busn_trx IS RECORD (
                business_id             ods_business_transactions.business_id%TYPE,
                business_sequence       ods_business_transactions.business_sequence%TYPE,
                document_family_code    ods_business_transactions.document_family_code%TYPE,
                family_ranking_order    ods_bsn_doc_tree_family_rank.family_ranking_order%TYPE,
                primary_costing_flag    VARCHAR2(1)
                );

        TYPE t_family_code_busn_tab IS TABLE OF t_family_code_busn_trx
        INDEX BY BINARY_INTEGER;

        SUBTYPE s_busn_sequence IS PLS_INTEGER;

        -- tabla de cadenas de business transactions
        TYPE t_busn_trx_bkp_tab IS TABLE OF t_busn_trx_tab
        INDEX BY BINARY_INTEGER;

        -- Private constant declarations

    	c_mill_ack_portfolio_type       CONSTANT ods_business_transactions.doc_portfolio_type%TYPE:= '88';
        c_post_process_portfolio_type   CONSTANT ods_business_transactions.doc_portfolio_type%TYPE:= '04';
        c_prod_operat_portfolio_type    CONSTANT ods_business_transactions.doc_portfolio_type%TYPE:= '06';
        c_sale_portfolio_type           CONSTANT ods_business_transactions.doc_portfolio_type%TYPE:= '02';
        c_invalid_busn_ref_id           CONSTANT ods_business_transactions.business_reference_id%TYPE:= -1;
        c_null_ack_doc_type             CONSTANT ods_business_document_tree.mill_ack_doc_type_code%TYPE:= '00';
        c_null_mill_ack_doc_num         CONSTANT ods_business_document_tree.mill_ack_doc_num%TYPE:= '00000000';
        --
        c_spij_comm_loc                 CONSTANT ods_business_transactions.doc_comm_location_code%TYPE:= 'SPI';
        c_tsa_comm_loc                  CONSTANT ods_business_transactions.doc_comm_location_code%TYPE:= 'TSA';
        c_kpt_comm_loc                  CONSTANT ods_business_transactions.doc_comm_location_code%TYPE:= 'KPT';
        --
        c_max_family_ranking_order      CONSTANT ods_bsn_doc_tree_family_rank.family_ranking_order%TYPE:= 99999999;

        c_doc_tree_twice_ins_root_desc  CONSTANT ods_busn_doc_tree_error_pkg.s_error_desc:= 'Regeneracion Business Trx: se esta dando de alta un nodo parent con un Business Id y Sequence ya existentes.';
        c_doc_tree_twice_ins_chld_desc  CONSTANT ods_busn_doc_tree_error_pkg.s_error_desc:= 'Regeneracion Business Trx: se esta dando de alta un nodo child con un Business Id y Sequence ya existentes.';
        c_doc_tree_twice_ins_ack_desc   CONSTANT ods_busn_doc_tree_error_pkg.s_error_desc:= 'Regeneracion Business Trx: se esta dando de alta un nodo conferma con un Business Id y Sequence ya existentes.';
        c_doc_tree_twice_ins_gen_desc   CONSTANT ods_busn_doc_tree_error_pkg.s_error_desc:= 'Regeneracion Business Trx: se esta dando de alta un nodo Generico con un Business Id y Sequence ya existentes.';
        c_busn_trx_ref_no_found_desc    CONSTANT ods_busn_doc_tree_error_pkg.s_error_desc:= 'Regeneracion Business Trx: no se encontro la cadena de Business Trx referenciado desde el nodo hijo S del Busn Document Tree.';
        c_no_data_found_on_regen_desc   CONSTANT ods_busn_doc_tree_error_pkg.s_error_desc:= 'Regeneracion Ref Business Trx: no se encontro algun componente en la regeneracion de cadenas que referencian a las Busn Trx modificadas';
        -- Private variable declarations

	--Variable interna para almacenar información de Completitud
	v_g_completeness_flag		ods_busn_tree_classifications.completeness_flag%TYPE;

        --VARIABLES GLOBALES ELIMINADAS PARA RESPETAR EL STANDARD y BEST PRACTICES TENARIS
        --Fueron reemplazadoas por el parseo de la FINISHING ORDER KEY que retorna el SP
        --Variables internas para almacenar información de Finishing
        --v_g_comm_loc            ods_business_transactions.doc_comm_location_code%TYPE;
        --v_g_doc_year            ods_business_transactions.document_year%TYPE;
        --v_g_doc_month           ods_business_transactions.document_month%TYPE;
        --v_g_doc_type            ods_business_transactions.doc_type_code%TYPE;
        --v_g_doc_num             ods_business_transactions.document_number%TYPE;
        --v_g_doc_itm_num         ods_business_transactions.document_item_num%TYPE;

        -- Function and procedure implementations

--////////////////// PRIVATE MODULES SPECIFICATION ////////////////////////

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   backup_busn_trx_sp
    Sistema: ODS
    Objetivo: dado un business id almacena la cadena de transacciones de la tabla ods_business_transactions
        en la tabla de cadena de business transaction
    Una vez almacenada la cadena se elimina la misma de la tabla ods_business_transactions

    Parámetros de entrada:
    p_business_id: identificador de business

    Parámetros de salida:
        p_busn_trx_bkp: tabla de cadena de business transaction en la que se
                almacena la cadena de transacciones
    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-10-13    t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE backup_busn_trx_sp(
    p_business_id    IN ods_business_doc_tree_ref_wrk.business_id%TYPE,
        p_busn_trx_bkp    IN OUT NOCOPY t_busn_trx_bkp_tab
        );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_busn_trx_ref_doc_tree_sp
    Sistema: ODS
    Objetivo: recupera una tabla plsql con las cadenas que deben ser actualizadas por
        business transactions refernciadas por las mismas que han sido modificadas
    Ademas de recuperar las business id este proceso almacena temporalmente las cadenas completas
        de estas business_id para que puedan ser actualizadas posteriormente, borrando luego las mismas
        de la tabla ods_business_transactions.
        Esto se debe a que las business_id especificadas en la tabla ODS_BUSINESS_DOC_TREE_WRK son referenciadas
        por estas cadenas impidiendo su borrado por referencias por FKs.
        De esta manera se borran las cadenas que las referencian, se procesan por el proceso build to order y son
        actualizadas y cargadas nuevamente en el proceso de regeneracion.

    Parámetros de entrada:

    Parámetros de salida:
        p_busn_trx_bkp: tabla de cadena de business transaction eliminados de la tabla ods_business_transactions
    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-20    t52585        Creacion de modulo
    2011-06-23    t52585        transformacion de insert de registros en la tabla ref work por insert or
                                update segun exista o no el business_id

*********************************************************************************************/
PROCEDURE get_busn_trx_ref_doc_tree_sp(
        p_busn_trx_bkp    OUT t_busn_trx_bkp_tab
        );


----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   remove_busn_trx_by_bid_sp
    Sistema: ODS
    Objetivo: borra los registros de la tabla de ods_business_transactions en base al
        business_id  que se especifica. Esto puede borrar mas de 1 registro

    Parámetros de entrada:
    p_business_id: identificador de business a borrar

    Parámetros de salida:


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-22    t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE remove_busn_trx_by_bid_sp(
        p_business_id    ods_business_transactions.business_id%TYPE
        );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   remove_busn_trx_by_bid_ref_sp
    Sistema: ODS
    Objetivo: borra los registros de la tabla de ods_business_transactions en base al
        business_id  y el business_reference_id que se especifica. Esto puede borrar mas de 1 registro

    Parámetros de entrada:
    p_business_id: identificador de business a borrar
    p_business_reference_id: identificador de business reference a borrar

    Parámetros de salida:


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-29    t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE remove_busn_trx_by_bid_ref_sp(
        p_business_id            IN ods_business_transactions.business_id%TYPE,
        p_business_reference_id     IN ods_business_transactions.business_reference_id%TYPE
        );

------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   insert_busn_trx_row_sp
    Sistema: ODS
    Objetivo: dado registro de la tabla ods_business_transactions, inserta el mismo en la tabla

    Parámetros de entrada:
        p_insert_busn_trx_tab_row: variable de tipo record de la tabla ods_business_transactions

    Parámetros de salida:


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-22    t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE insert_busn_trx_row_sp(
        p_insert_busn_trx_tab_row IN t_busn_trx_tab_row
        );

------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   insert_busn_trx_row_sp
    Sistema: ODS
    Objetivo: dada una tabla plsql con registros de la tabla ods_business_transactions, inserta
        los mismos en la tabla

    Parámetros de entrada:
        p_insert_busn_trx_tab_row: variable de tipo record de la tabla ods_business_transactions

    Parámetros de salida:


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-22    t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE insert_busn_trx_row_sp(
        p_busn_trx_tab IN t_busn_trx_tab
        );

------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   add_busn_trx_root_record_sp
    Sistema: ODS
    Objetivo: dado un registro de la tabla ods_business_document_tree se da de alta en la tabla
        ods_business_transactions un registro padre en la secuencia completando los datos desde
        los datos de nodo padre (root) del arbol de documentos

    Parámetros de entrada:
        p_busn_doc_tree_tab_row: variable de tipo record de la tabla ods_business_document_tree
    p_busn_sequence: secuencia dentro las business transactions
    p_process_name : nombre del proceso que da de alta el registro a acentarse en la auditoria
    p_process_date : fecha en que se da de alta el registro a acentarse en la auditoria


    Parámetros de salida:


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-27    t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE add_busn_trx_root_record_sp(
        p_busn_doc_tree_tab_row    IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_busn_sequence            IN s_busn_sequence,
        p_process_name            IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date            IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        );

------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   add_busn_trx_ack_record_sp
    Sistema: ODS
    Objetivo: dado un registro de la tabla ods_business_document_tree se da de alta en la tabla
        ods_business_transactions un registro hijo en la secuencia completando los datos desde
        los datos de nodo conferma del arbol de documentos

    Parámetros de entrada:
        p_busn_doc_tree_tab_row: variable de tipo record de la tabla ods_business_document_tree
        p_busn_sequence: secuencia dentro las business transactions
        p_mill_ack_portfolio_type: portfolio type a asignar al nodo conferma
    p_process_name : nombre del proceso que da de alta el registro a acentarse en la auditoria
        p_process_date : fecha en que se da de alta el registro a acentarse en la auditoria


    Parámetros de salida:


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-27    t52585        Creacion de modulo
    2012-01-31      t52585    Se agrega un parametro con el portfolio type a asignar al nodo conferma a dar de alta
                    segun las condiciones de ODS_BT_SOCIETIES recuperados
    ----------------------------------------------------------------------------------------------------------------------------------------------
    2015-11-19          T53605 (Pedro Dias)             Modificacion del modulo para que aquellos nodos en los que la familia del hijo no venga informada por la
                                                        Document Tree tome el datos de la conferma, es decir el valor del padre.

*********************************************************************************************/
PROCEDURE add_busn_trx_ack_record_sp(
        p_busn_doc_tree_tab_row        IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_busn_sequence                IN s_busn_sequence,
        p_mill_ack_portfolio_type    IN ods_business_document_tree.doc_portfolio_type%TYPE,
        p_process_name                IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date                IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        );

------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   add_busn_trx_add_ack_record_sp
    Sistema: ODS
    Objetivo: dado un registro de la tabla ods_business_document_tree se da de alta en la tabla
        ods_business_transactions un registro hijo en la secuencia completando los datos desde
        los datos de nodo conferma adicional del arbol de documentos

    Parámetros de entrada:
        p_busn_doc_tree_tab_row: variable de tipo record de la tabla ods_business_document_tree
        p_busn_sequence: secuencia dentro las business transactions
        p_mill_ack_portfolio_type: portfolio type a asignar al nodo conferma
    p_process_name : nombre del proceso que da de alta el registro a acentarse en la auditoria
        p_process_date : fecha en que se da de alta el registro a acentarse en la auditoria


    Parámetros de salida:


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2012-01-31    t52585        Creacion de modulo
    2012-06-27      t31105    Se agrega un parametro con el portfolio type a asignar al parent del nodo conferma a dar de alta
                    segun las condiciones de ODS_BT_SOCIETIES recuperados
    ----------------------------------------------------------------------------------------------------------------------------------------------
    2015-11-19          T53605 (Pedro Dias)             Modificacion del modulo para que aquellos nodos en los que la familia del hijo no venga informada por la
                                                        Document Tree tome el datos de la conferma, es decir el valor del padre.

*********************************************************************************************/
PROCEDURE add_busn_trx_add_ack_record_sp(
        p_busn_doc_tree_tab_row         IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_busn_sequence                 IN s_busn_sequence,
        p_mill_ack_portfolio_type       IN ods_business_document_tree.doc_portfolio_type%TYPE,
        p_process_name                  IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date                  IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        );
------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   add_busn_trx_child_record_sp
    Sistema: ODS
    Objetivo: dado un registro de la tabla ods_business_document_tree se da de alta en la tabla
        ods_business_transactions un registro hijo en la secuencia completando los datos desde
        los datos de nodo hijo (no conferma) del arbol de documentos

    Parámetros de entrada:
        p_busn_doc_tree_tab_row: variable de tipo record de la tabla ods_business_document_tree
    p_busn_sequence: secuencia dentro las business transactions
        p_process_name : nombre del proceso que da de alta el registro a acentarse en la auditoria
        p_process_date : fecha en que se da de alta el registro a acentarse en la auditoria

    Parámetros de salida:


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-27    t52585        Creacion de modulo
    ----------------------------------------------------------------------------------------------------------------------------------------------
    2015-11-19          T53605 (Pedro Dias)             Modificacion del modulo para que aquellos nodos en los que la familia del hijo no venga informada por la
                                                        Document Tree tome el datos del padre.

*********************************************************************************************/
PROCEDURE add_busn_trx_child_record_sp(
        p_busn_doc_tree_tab_row IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_busn_sequence         IN s_busn_sequence,
        p_process_name          IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date          IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        );

------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   add_build_stk_busn_trx_seq_sp
    Sistema: ODS
    Objetivo: dado un registro de business document tree de tipo link S
        se busca la cadena de business transaction correspondiente y se inserta
        en la cadena original.
        Si no se encuentra se registra la business id para reproceso. Cuando se llega al tercer reproceso
        se elimina de la tabla ods_business_doc_tree_wrk y se informa en el log de errores.

    Parámetros de entrada:
        p_busn_doc_tree_tab_row: variable de tipo record de la tabla ods_business_document_tree
    p_busn_sequence: secuencia dentro las business transactions
        p_process_name : nombre del proceso que da de alta el registro a acentarse en la auditoria
        p_process_date : fecha en que se da de alta el registro a acentarse en la auditoria

    Parámetros de salida:
    p_busn_sequence: sequencia dentro de la cadena de business transaction actualizada segun
    los registros insertados en la cadena.

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-27    t52585        Creacion de modulo
    ----------------------------------------------------------------------------------------------------------------------------------------------
    2015-11-19          T53605 (Pedro Dias)             Modificacion del modulo para que aquellos nodos en los que la familia del hijo no venga informada por la
                                                        Document Tree tome el datos del padre.

*********************************************************************************************/
PROCEDURE add_build_stk_busn_trx_seq_sp(
        p_busn_doc_tree_tab_row IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_busn_sequence         IN OUT s_busn_sequence,
        p_process_name          IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date          IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        );
------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   add_pure_build_stk_busn_trx_sp
    Sistema: ODS
    Objetivo: dado un registro de business document tree de tipo link S
        se busca la cadena de business transaction correspondiente y se inserta
        en la cadena original.
        Si no se encuentra se registra la business id para reproceso. Cuando se llega al tercer reproceso
        se elimina de la tabla ods_business_doc_tree_wrk y se informa en el log de errores.

    Parámetros de entrada:
        p_busn_doc_tree_tab_row: variable de tipo record de la tabla ods_business_document_tree
    p_busn_sequence: secuencia dentro las business transactions
        p_process_name : nombre del proceso que da de alta el registro a acentarse en la auditoria
        p_process_date : fecha en que se da de alta el registro a acentarse en la auditoria

    Parámetros de salida:
    p_busn_sequence: sequencia dentro de la cadena de business transaction actualizada segun
    los registros insertados en la cadena.

    Notas:
    Autor: T52585
    Historia:
    Fecha               Autor                           Descripción
    2011-07-13          t52585                          Creacion de modulo
    ----------------------------------------------------------------------------------------------------------------------------------------------
    2015-11-19          T53605 (Pedro Dias)             Modificacion del modulo para que aquellos nodos en los que la familia del hijo no venga informada de la
                                                        Document Tree tome el datos del padre.

*********************************************************************************************/
PROCEDURE add_pure_build_stk_busn_trx_sp(
        p_busn_doc_tree_tab_row IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_target_business_id    IN ods_business_transactions.business_id%TYPE,
        p_process_name          IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date          IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        );

------------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   process_build_to_order_trx_sp
    Sistema: ODS
    Objetivo: dado un business id regenera la cadena de business transaction correspondiente
        recuperando la nueva cadena desde el arbol de documentos

    Parámetros de entrada:
        p_business_id: identificador de business de la cadena a procesar

    Parámetros de salida:

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor                          Descripción
    2010-09-27  t52585                  Creacion de modulo
    2011-07-12  t52585                  Aplicacion de modificaciones release 2: Unificacion de cadenas puro Stock
    2012-01-27  t52585                  Pasaje a los procesos de alta de nodo raiz, conferma e hijo fecha CREATED_BY
                                        de nodo de arbol en lugar del nombre del proceso
    ----------------------------------------------------------------------------------------------------------------------------------------------
    2015-09-24  T53605 (Pedro Dias)     Ver documentacion de PROCESS_BUILD_TO_ORDER_TRX_SP
                                        Modificacion del SP que genera los enganches de los nodos de la Document Tree en la regeneracion de las cadenas de BT
                                        Los cambios aplicados son:
                                                1) Se agrega mas detalle al logueo en la cancelacion del proceso cuando existen nodos erroneos en la Doc Tree
                                                2) Se corrigió el issue para evitar la cancelacion de valores erroneo de las confermas
                                                3) Se corrigió el issue para evitar la cancelacion en negocios mixtos donde el link Stock (tipo S) no puede
                                                   engancharse a la parte BTO.
                                                4) Se agrega al logueo los BID que tienen valores erroneos para que sean detectados en algun DQM con mayor facilidad.
    ----------------------------------------------------------------------------------------------------------------------------------------------
    2015-11-19          T53605 (Pedro Dias)             Modificacion del modulo para que aquellos nodos en los que la familia del hijo no venga informada por la
                                                        Document Tree tome el datos de la conferma, es decir el valor del padre.

*********************************************************************************************/
PROCEDURE process_build_to_order_trx_sp(
        p_business_id    IN ods_business_doc_tree_wrk.business_id%TYPE
        );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_busn_id_sp
    Sistema: ODS
    Objetivo: verfifica la existencia de un business transaction a partir de los datos de
        identificacion de un registro. En caso contrario retorna NULL.
        Dado que un documento puede estar siendo refereciado en varias cadenas se busca
        la instancia del documento original, el cual no es referencia de ninguna otra cadena.
        Business_reference_id NULO

    Parámetros de entrada:
        p_doc_comm_location_code: Identificador unívoco de una sociedad Tenaris (dominio TEN)
        p_document_year: Año correspondiente a la fecha de alta del documento
        p_document_month: mes correspondiente a la fecha de alta del documento
        p_doc_type_code: Tipo de documento comercial (Orden de venta, Orden de compra, etc)
        p_document_num: Número del documento
        p_document_item_num: Identificador de una posición del documento
        p_document_split_num: Identificador de una de las aperturas de la posición del documento
    p_historical_flag: String Booleano que indica si se deben tener en cuenta los nodos histoticos o no. Por defecto se tienen en cuenta todos.

    Parámetros de salida:
        p_business_id    : identificador de business
    p_business_sequence: secuencia dentro de la cadena

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-08-03    T52585        Creacion de procedimiento
    2011-12-21    T52585    Agregado de condicion nodo historico
*********************************************************************************************/
PROCEDURE get_busn_id_sp(
        p_doc_comm_location_code        IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_document_year                 IN ods_business_transactions.document_year%TYPE,
        p_document_month                IN ods_business_transactions.document_month%TYPE,
        p_doc_type_code                 IN ods_business_transactions.doc_type_code%TYPE,
        p_document_number               IN ods_business_transactions.document_number%TYPE,
        p_document_item_num             IN ods_business_transactions.document_item_num%TYPE,
        p_document_split_num            IN ods_business_transactions.document_split_num%TYPE,
        p_historical_flag               IN ods_common_pkg.s_str_boolean DEFAULT ods_common_pkg.c_str_true,
        p_business_id                   OUT ods_business_transactions.business_id%TYPE,
        p_business_sequence             OUT ods_business_transactions.business_sequence%TYPE
        );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   exist_ref_busn_trx_fn
    Sistema: ODS
    Objetivo: verfifica la existencia de un business transaction referenciado dentro de una cadena
        a partir de los datos de business_id y la identificacion de un nodo. En caso contrario
        retorna NULL.
        Para identificar los casos en que se esta referenciando a un nodo se debe verificar
        Business_reference_id NO NULO y Business_reference_seq = 1

    Parámetros de entrada:
        p_business_id: identificador de business
        p_doc_comm_location_code: Identificador unívoco de una sociedad Tenaris (dominio TEN)
        p_document_year: Año correspondiente a la fecha de alta del documento
        p_document_month: mes correspondiente a la fecha de alta del documento
        p_doc_type_code: Tipo de documento comercial (Orden de venta, Orden de compra, etc)
        p_document_num: Número del documento
        p_document_item_num: Identificador de una posición del documento
        p_document_split_num: Identificador de una de las aperturas de la posición del documento

    Parámetros de salida:
        Salida de funcion:
            TRUE en caso en que se encuentre la cadena
            FALSE en caso contrario

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2011-07-13    T52585        Creacion de procedimiento
    2012-01-15    T52585    Eliminacion de condicion BUSINESS_REFERENCE_ID NULL

*********************************************************************************************/
FUNCTION exist_ref_busn_trx_fn(
        p_business_id                   IN ods_business_transactions.business_id%TYPE,
        p_doc_comm_location_code        IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_document_year                 IN ods_business_transactions.document_year%TYPE,
        p_document_month                IN ods_business_transactions.document_month%TYPE,
        p_doc_type_code                 IN ods_business_transactions.doc_type_code%TYPE,
        p_document_number               IN ods_business_transactions.document_number%TYPE,
        p_document_item_num             IN ods_business_transactions.document_item_num%TYPE,
        p_document_split_num            IN ods_business_transactions.document_split_num%TYPE
        )
RETURN BOOLEAN;

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_row_busn_trx_sp
    Sistema: ODS
    Objetivo: dado el identificador de business retorna en un ref cursor el conjuto de business
        transaction que pertenecen el mismo business de la tabla ods_business_transaction ordenados
        por su secuencia

    Parámetros de entrada:
        p_business_id: Identificador del business de todos los registros del business transaction
        p_business_sequence: sequencia desde la cual se desea recuperar la cadena

    Parámetros de salida:
    p_cur_busn_trx: ref cursor con las business transaction que pertenecen al mismo business id

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-27    t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE get_row_busn_trx_sp(
        p_business_id        IN ods_business_transactions.business_id%TYPE,
        p_business_sequence        IN ods_business_transactions.business_sequence%TYPE,
        p_cur_busn_trx             OUT t_refcursor
        );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_row_busn_trx_sp
    Sistema: ODS
    Objetivo: dado el identificador de business retorna en una tabla plsql el conjuto de business
        transaction que pertenecen el mismo business de la tabla ods_business_transaction ordenados
        por su secuencia

    Parámetros de entrada:
        p_business_id: Identificador del business de todos los registros del business transaction

    Parámetros de salida:
    p_busn_trx_tab: tabla plsql con las business transaction que pertenecen al mismo business id


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-27    t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE get_row_busn_trx_sp(
        p_business_id   IN ods_business_transactions.business_id%TYPE,
        p_busn_trx_tab  OUT t_busn_trx_tab
        );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   regenerate_ref_busn_trx_sp
    Sistema: ODS
    Objetivo: dado el conjuto de cadenas de transacciones a regenerar por cambios en las cadenas
        referenciadas inserta los registros en la tabla ods business transaction, reemplazando las
        cadenas referenciadas por las que se regeneraron.

    Parámetros de entrada:
        p_busn_trx_tab: tabla plsql de cadenas.

    Parámetros de salida:


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-10-08    t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE regenerate_ref_busn_trx_sp(
        p_busn_trx_tab     IN t_busn_trx_tab
        );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   proc_changed_ref_busn_trx_sp
    Sistema: ODS
    Objetivo: dado una tabla con las las cadenas que deben ser actulizadas dado que las cadenas
        que hacen referencia cambiaron, se toman los datos de la tabla y se regenera la cadena en la
        tabla ODS_BUSINESS_TRANSACTION actualizando ademas las cadenas que cabiaron
        Las cadenas de transacciones que cambiaron deben ser reemplazados con los nuevos y si es necesario
        reenumerar la secuencia de las transacciones que quedan.

    Parámetros de entrada:
    p_busn_trx_bkp_tab    tabla de cadenas a actualizar

    Parámetros de salida:


    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2010-09-29    t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE proc_changed_ref_busn_trx_sp(
        p_busn_trx_bkp_tab    IN t_busn_trx_bkp_tab
        );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_last_busn_trx_node_sp
    Sistema: ODS
    Objetivo: dado el identificador de business retorna el ultimo nodo de business transaction al que
    pertenece. Es decir el nodo del mismo business_id de secuencia mayor

    Parámetros de entrada:
        p_business_id: Identificador del business de todos los registros del business transaction

    Parámetros de salida:
    p_busn_trx_tab_row: registro del tipo de la tabla ods_business_transactions correspondiente al
        ultimo nodo de la cadena

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2011-07-14   t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE get_last_busn_trx_node_sp(
        p_business_id           IN ods_business_transactions.business_id%TYPE,
        p_busn_trx_tab_row      OUT t_busn_trx_tab_row
        );


----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_fam_busn_trx_child_nodes
    Sistema: ODS
    Objetivo: dado un nodo de un Business Transaction retorna en un cursor los nodos hijos inmediatos con
    el ranking asignado segun la familia de producto al que pertenece la orden asociada
    a los nodos hijos.

    Parámetros de entrada:
        p_parent_busn_trx_tab_row: nodo del business transaction desde el cual se recuperaran los nodos
        hijos

    Parámetros de salida:
    p_child_busn_trx_tab: tabla plsql con nodos hijos

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2011-07-15   t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE get_fam_busn_trx_child_nodes(
    p_parent_busn_trx_tab_row    IN t_busn_trx_tab_row,
    p_child_busn_trx_tab        OUT ods_business_transaction_pkg.t_family_code_busn_tab
        );



----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_post_proc_node_sp
    Sistema: ODS
    Objetivo: dado un identificador business_id y una secuencia, retorna el primer nodo de tipo
    post proceso (portfolio type = '04') para el business_id espécificado y a partir de la secuencia
    dada.

    Parámetros de entrada:
        p_business_id: identificador de business
        p_business_sequence: secuencia dentro del business transaction

    Parámetros de salida:
    p_busn_trx_tab_row: registro de business transaction correspondiente a un nodo de pos proceso

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2011-07-16   t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE get_post_proc_node_sp (
    p_business_id        IN ods_business_transactions.business_id%TYPE,
        p_business_sequence    IN ods_business_transactions.business_sequence%TYPE,
        p_busn_trx_tab_row      OUT t_busn_trx_tab_row
        );


----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   mark_main_busn_id_cost_brch_sp
    Sistema: ODS
    Objetivo: dado un identificador business_id y una secuencia identifica y marca las ramas secundarias
    para el costeo a partir del business id y la secuencia especificada

    Parámetros de entrada:
        p_business_id: identificador de business
        p_business_sequence: secuencia dentro del business transaction

    Parámetros de salida:

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2011-07-16   t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE mark_main_busn_id_cost_brch_sp(
        p_business_id        IN ods_business_transactions.business_id%TYPE,
        p_business_sequence    IN ods_business_transactions.business_sequence%TYPE
        );


----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   mark_main_costing_branch_sp
    Sistema: ODS
    Objetivo: identifica y marca las ramas secundarias para el costeo para todos
    los business_id especificados en la tabla de trabajo ODS_BUSINESS_DOC_TREE_WRK

    Parámetros de entrada:

    Parámetros de salida:

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2011-07-16   t52585        Creacion de modulo

*********************************************************************************************/
PROCEDURE mark_main_costing_branch_sp;

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_build_to_stock_busn_id_sp
    Sistema: ODS
    Objetivo: verfifica la existencia de un business transaction a partir de los datos de
        identificacion de un registro. En caso contrario retorna NULL. Esta cadena debe ser
    una cadena Build to Stock, es decir, debe tener al menos una cadena
    referenciada (BUSINESS_REFERNCE_ID <> NULL).
        Dado que un documento en una cadena Buil to Stock puede estar presente (refereciado)
    en varias cadenas se retorna una estructura de tipo tabla PLSQL que especifica el
    business_id y el business_sequence en que se encontro el documento
    Si no encuentra ninguno se retorna la estructura sin ningun elemento

    Parámetros de entrada:
        p_doc_comm_location_code: Identificador unívoco de una sociedad Tenaris (dominio TEN)
        p_document_year: Año correspondiente a la fecha de alta del documento
        p_document_month: mes correspondiente a la fecha de alta del documento
        p_doc_type_code: Tipo de documento comercial (Orden de venta, Orden de compra, etc)
        p_document_num: Número del documento
        p_document_item_num: Identificador de una posición del documento
        p_document_split_num: Identificador de una de las aperturas de la posición del documento

    Parámetros de salida:
    Tabla plsql (t_business_trx_id_tab) compuesta por elementos con las siguientes columnas
          p_business_id    : identificador de business
          p_business_sequence: secuencia dentro de la cadena

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2011-12-20   T52585        Creacion de procedimiento

*********************************************************************************************/
PROCEDURE get_build_to_stock_busn_id_sp(
    p_doc_comm_location_code     IN ods_business_transactions.doc_comm_location_code%TYPE,
    p_document_year            IN ods_business_transactions.document_year%TYPE,
    p_document_month            IN ods_business_transactions.document_month%TYPE,
    p_doc_type_code                IN ods_business_transactions.doc_type_code%TYPE,
    p_document_number            IN ods_business_transactions.document_number%TYPE,
    p_document_item_num            IN ods_business_transactions.document_item_num%TYPE,
    p_document_split_num            IN ods_business_transactions.document_split_num%TYPE,
    p_business_trx_id_tab              OUT t_business_trx_id_tab
    );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   busn_doc_tree_man_to_trx_sp
    Sistema: ODS
    Objetivo: dado un registro de Business Document Tree Manual retorna el conjunto de business transactions.
    NOTA: por ahora solo se va a tener en cuenta la posibilidad de que un nodo de doc tree se transforme en un
    unico nodo Business Transaction, es decir, para los casos Nodo parent - Nodo Conferma y Nodo Parent - Nodo Child, donde
    en ningun caso se genera un nodo Business Transaction para el nodo parent ni existe el caso en que se encuentran
    presentes los 3 tipos de nodos en un mismo registro

    Parámetros de entrada:
    p_busn_doc_tree_man_row: registro de Business Document Tree manual

    Parámetros de salida:
    p_busn_trx_row: registro de tipo t_busn_trx_tab_row

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2011-12-22   T52585        Creacion de procedimiento

*********************************************************************************************/
PROCEDURE busn_doc_tree_man_to_trx_sp(
    p_busn_doc_tree_man_row     IN ods_business_doc_tree_man_pkg.t_doc_tree_man_tab_row,
    p_busn_trx_row              OUT t_busn_trx_tab_row
    );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_empty_busn_trx_row_cur_fn
    Sistema: ODS
    Objetivo: retorna un ref cursor con una query con el formato de registro ods_busn_trx_row_type
        sin registros

    Parámetros de entrada:

    Parámetros de salida:
    Salida de Funcion: identificador de ref cursor

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2011-12-23   T52585        Creacion de procedimiento

*********************************************************************************************/
FUNCTION get_empty_busn_trx_row_cur_fn
RETURN ods_common_pkg.t_refcursor;


----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_busn_trx_row_cur_fn
    Sistema: ODS
    Objetivo: dada una tabla PLSQL de tipo t_busn_trx_table con registros de tipo t_busn_trx_tab_row retorna un
    ref cursor con una query con el formato de registro ods_busn_trx_row_type

    Parámetros de entrada:
    p_busn_trx_table: tabla plsql con registros de tipo t_busn_trx_tab_row

    Parámetros de salida:
    Salida de Funcion: identificador de ref cursor

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2011-12-23   T52585        Creacion de procedimiento

*********************************************************************************************/
FUNCTION get_busn_trx_row_cur_fn(
    p_busn_trx_table    t_busn_trx_tab
        )
RETURN ods_common_pkg.t_refcursor;


----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   find_busn_trx_node_seq_fn
    Sistema: ODS
    Objetivo: verfifica la existencia de un nodos business transaction dentro de una cadena
        a partir de los datos de business_id y la identificacion de un nodo.
        En caso de exisitir retorna el numero de sequencia en que se encuentra, si no lo encuntra
        retorna NULL

    Parámetros de entrada:
        p_business_id: identificador de business
        p_doc_comm_location_code: Identificador unívoco de una sociedad Tenaris (dominio TEN)
        p_document_year: Año correspondiente a la fecha de alta del documento
        p_document_month: mes correspondiente a la fecha de alta del documento
        p_doc_type_code: Tipo de documento comercial (Orden de venta, Orden de compra, etc)
        p_document_num: Número del documento
        p_document_item_num: Identificador de una posición del documento
        p_document_split_num: Identificador de una de las aperturas de la posición del documento

    Parámetros de salida:
        Salida de funcion:
            secuencia en quie se encuentra el nodo buscado, si no lo encuentra retorna NULL

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2012-03-15    T52585        Creacion de procedimiento

*********************************************************************************************/
FUNCTION find_busn_trx_node_seq_fn(
    p_business_id            IN ods_business_transactions.business_id%TYPE,
        p_doc_comm_location_code    IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_document_year            IN ods_business_transactions.document_year%TYPE,
        p_document_month            IN ods_business_transactions.document_month%TYPE,
        p_doc_type_code                IN ods_business_transactions.doc_type_code%TYPE,
        p_document_number            IN ods_business_transactions.document_number%TYPE,
        p_document_item_num            IN ods_business_transactions.document_item_num%TYPE,
        p_document_split_num            IN ods_business_transactions.document_split_num%TYPE
        )
RETURN ods_business_transactions.business_sequence%TYPE;

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   generate_parent_busn_trx_sp
    Sistema: ODS
    Objetivo: dado un registro de Business Document Tree Manual toma la porcion de informacion
    correspondiente al nodo parent y genera un nodo business transaction de tipo root, solo
    con los datos del documento

    Parámetros de entrada:
    p_busn_doc_tree_man_row: registro de Business Document Tree manual

    Parámetros de salida:
    p_busn_trx_row: registro de tipo t_busn_trx_tab_row

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2012-03-15   T52585       Creacion de procedimiento

*********************************************************************************************/
PROCEDURE generate_parent_busn_trx_sp(
    p_busn_doc_tree_man_row     IN ods_business_doc_tree_man_pkg.t_doc_tree_man_tab_row,
    p_busn_trx_row              OUT t_busn_trx_tab_row
    );


----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   delete_error_busn_id_sp
    Sistema: ODS
    Objetivo: se eliminan las business_id de la tabla ods_business_doc_tree_wrk que dieron error para
    para que los mismos no lleguen a los procesos posteriores

    Parámetros de entrada:

    Parámetros de salida:

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2012-06-19   T52585       Creacion de procedimiento

*********************************************************************************************/
PROCEDURE delete_error_busn_id_sp;

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   invalid_parent_millack_node_fn
    Sistema: ODS
    Objetivo: dado un nodo de business document tree se verfica si se trata de un nodo parent/millack invalido
    En particular se esta verificando el caso de metalmecanica que poseee nodo de este tipo con numero de
    nodo conferman invalido

    Parámetros de entrada:

    Parámetros de salida:

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2012-06-21   T52585       Creacion de procedimiento

*********************************************************************************************/
FUNCTION invalid_parent_millack_node_fn(
        p_busn_doc_tree_tab_row IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row
        )
RETURN BOOLEAN;


----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   update_bt_serv_sale_sp
    Sistema: ODS
    Objetivo: se actualiza el business_id  del registro de la tabla ODS_BT_SERVICE_SALES correspondiente a
    los parametros especiifcados

    Parámetros de entrada:
        p_ss_comm_loc_code: commercial location del service sale
        p_ss_order_year: año del service sale
        p_ss_order_month: mes del service sale
        p_ss_doc_type_code: tipo de documento del service sale
        p_ss_number: numero de documento del service sale
        p_ss_item_num: numero del item del documento del service sale
        p_ss_split_num: numero del split del documento del service sale
        p_business_id: business id a asignar al documento del service sale

    Parámetros de salida:

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2012-10-11  T52585       Creacion de procedimiento

*********************************************************************************************/
PROCEDURE update_bt_serv_sale_sp(
        p_ss_comm_loc_code    IN ods_bt_service_sales.ss_comm_loc_code%TYPE,
        p_ss_order_year       IN ods_bt_service_sales.ss_order_year%TYPE,
        p_ss_order_month      IN ods_bt_service_sales.ss_order_month%TYPE,
        p_ss_doc_type_code    IN ods_bt_service_sales.ss_doc_type_code%TYPE,
        p_ss_number           IN ods_bt_service_sales.ss_number%TYPE,
        p_ss_item_num         IN ods_bt_service_sales.ss_item_num%TYPE,
        p_ss_split_num        IN ods_bt_service_sales.ss_split_num%TYPE,
        p_business_id        IN ods_bt_service_sales.business_id%TYPE
        );


----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   generate_doc_bt_serv_sale_sp
    Sistema: ODS
    Objetivo: dado un identificador de un registro de la tabla ODS_BT_SERVICE_SALES da de alta el nodo de Service Sale
    en la cadena correspondiente.
    Ademas asigna el business_id de la cadena en la que se agrego el nodo al registro de la orden de BT Servce Sale

    Parámetros de entrada:
        p_bt_service_sale_rec: variable de tipo record que posee los campos de la tabla ODS_BR_SERVICE_SALES
                ss_comm_loc_code: commercial location del service sale
                ss_order_year: año de la SO asociada
                ss_order_month: mes de la SO asociada
                ss_doc_type_code: tipo de documento de la SO asociada
                ss_number: numero de documento de la SO asociada
                ss_item_num: numero del item del documento de la SO asociada
                ss_split_num: numero del split del documento de la SO asociada
                business_id: identificador de business
                so_comm_loc_code: commercial location de la SO asociada
                so_order_year: año de la SO asociada
                so_order_month: mes de la SO asociada
                so_doc_type_code: tipo de documento de la SO asociada
                so_number: numero de documento de la SO asociada
                so_item_num: numero del item del documento de la SO asociada
                so_split_num: numero del split del documento de la SO asociada

    Parámetros de salida:

    Notas:
    Autor: T52585
    Historia:
    Fecha               Autor                           Descripción
    2012-10-11          T52585                  Creacion de procedimiento
    -------------------------------------------------------------------------------------------
    2015-10-19          T53605 - Pedro Dias     Modificacion del SP:
                                                Agregar la familia a la variable del record type que luego se inserta en la tabla BT,
                                                la familia proviene de ODS_BT_SERVICE_SALES

*********************************************************************************************/
PROCEDURE generate_doc_bt_serv_sale_sp(
        p_bt_service_sale_rec    IN t_bt_service_sale_rec
    );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   generate_bt_serv_sale_sp
    Sistema: ODS
    Objetivo: dar de alta el nodo de Service Sale para todos los registros de la tabla ODS_BT_SERVICE_SALES
    que no tiene especificado el business_id (tiene especificado 9999999999)
    Ademas asigna el business_id de la cadena en la que se agrego el nodo al registro de la orden de BT Servce Sale

    Parámetros de entrada:

    Parámetros de salida:

    Notas:
    Autor: T52585
    Historia:
    Fecha               Autor                           Descripción
    2012-10-11          T52585                  Creacion de procedimiento
    -------------------------------------------------------------------------------------------
    2015-10-19          T53605 - Pedro Dias     Modificacion del SP:
                                                Agregar columna de familia al cursor cur_bt_serv_sales que apunta a la tabla ODS_BT_SERVICE_SALES
	                                        Modificar el TYPE ODS_BUSINESS_TRANSACTION_PKG.t_bt_service_sale_rec

*********************************************************************************************/
PROCEDURE generate_bt_serv_sale_sp;

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   generate_doc_g17_conf_order_sp
    Sistema: ODS
    Objetivo: dado un Business_Id, da de alta el nodo de Conforming Order en la cadena correspondiente.

    Parámetros de entrada:
        p_g17_business_id: identificador de business

    Parámetros de salida:

    Notas:
    Autor: T32732
    Historia:
    Fecha        Autor        Descripción
    2014-11-28   T32732       Creacion de procedimiento

*********************************************************************************************/
PROCEDURE generate_doc_g17_conf_order_sp(
        p_g17_business_id    IN ods_business_transactions.BUSINESS_ID%type
    );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   generate_g17_conf_order_sp
    Sistema: ODS
    Objetivo: Completar cadenas con origen en 'G17 - METALMECANICA', insertando en la cadena de docuemntos
    el nodo correspondiente a la Conforming Order. Esto se debe a la integración que hoy existe entre TEN
    y el sistema Legacy, donde en la mayoría de los casos la orden del TEN queda desvinculada de la orden
    del legacy (Conforming Order)

    Parámetros de entrada:

    Parámetros de salida:

    Notas:
    Autor: T32732
    Historia:
    Fecha               Autor                           Descripción
    2014-11-28          T32732                  Creacion de procedimiento
    -------------------------------------------------------------------------------------------
    2015-10-19          T53605 - Pedro Dias     Modificacion del SP:
                                                Agregar la familia a la variable del record type que luego se inserta en la tabla BT

*********************************************************************************************/
PROCEDURE generate_g17_conf_order_sp;


----------------------------------------------------------------------------------------------

/*********************************************************************************************
Nombre del programa:   generate_doc_link_extornos_sp
    Sistema: ODS
    Objetivo: Dados los parámetros de entradas, recupera la cadena de documentos, e inserta al
    final de la misma el nodo correspondiente a la Orden de Producción que fabricó los tubos,
    realizando la vinculación con esta otra cadena mediante el BUSINESS_ID_REFERENCE.

    Parámetros de entrada:
        p_ext_parent_doc_comm_loc_code
        p_ext_parent_document_year
        p_ext_parent_document_month
        p_ext_parentdocument_number
        p_ext_parentdocument_item_num
        p_ext_parentdocument_split_num
        p_ext_child_doc_comm_loc_code
        p_ext_child_doc_year
        p_ext_child_doc_month
        p_ext_child_doc_number
        p_ext_child_doc_item_num
        p_ext_child_doc_split_num
        p_extorno_doc_type_code


    Parámetros de salida:

    Notas:
    Autor: T32732
    Historia:
    Fecha        Autor                  Descripción
    ---------------------------------------------------
    2015-03-19   T32732                 Creacion de procedimiento
    ---------------------------------------------------
    2015-08-19  T53605 (Pedro Dias)     Cambio de firma, se renombran los parametros y el orden en que deben ser enviados desde el SP que lo invoca.
                                        Identacion y nomenclacion del SP para tener legibilidad en la lectura del codigo.
                                        Para obtener el BID y la Sequence de la orden correspondiente al Extorno (Tanto PARENT como CHILD) se invoca a la
                                        funcion GET_BUSN_ID_SP con un DOC_TYPE_CODE = '00' .
                                        En funcion al parametro que contiene el DOC_TYPE_CODE del Extorno ('IM') se determina si se asigna 'Y'
                                        al SECONDARY_COSTING_FLAG de la BT.
    ---------------------------------------------------

*********************************************************************************************/
PROCEDURE generate_doc_link_extornos_sp(
        p_ext_parent_doc_comm_loc_code  IN  ods_business_transactions.parent_doc_comm_loc%TYPE,
        p_ext_parent_doc_year           IN  ods_business_transactions.parent_doc_year%TYPE,
        p_ext_parent_doc_month          IN  ods_business_transactions.parent_doc_month%TYPE,
        p_ext_parent_doc_number         IN  ods_business_transactions.parent_doc_number%TYPE,
        p_ext_parent_doc_item_num       IN  ods_business_transactions.parent_doc_item_num%TYPE,
        p_ext_parent_doc_split_num      IN  ods_business_transactions.parent_doc_split_num%TYPE,
        p_ext_child_doc_comm_loc_code   IN  ods_business_transactions.doc_comm_location_code%TYPE,
        p_ext_child_doc_year            IN  ods_business_transactions.document_year%TYPE,
        p_ext_child_doc_month           IN  ods_business_transactions.document_month%TYPE,
        p_ext_child_doc_number          IN  ods_business_transactions.document_number%TYPE,
        p_ext_child_doc_item_num        IN  ods_business_transactions.document_item_num%TYPE,
        p_ext_child_doc_split_num       IN  ods_business_transactions.document_split_num%TYPE,
        p_extorno_doc_type_code         IN  ods_business_transactions.doc_type_code%TYPE
        );

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   generate_bt_link_extornos_sp
    Sistema: ODS
    Objetivo: registrar en la cadena de documentos las relaciones entre órdenes del legacy (confermas)
    producidas por extornos de stock, a fin de lograr trazabilidad hasta la orden de producción que
    conformó el tubo. Por cada registro existente en la tabla ODS_BUSN_TRANSACTION_EXTORNOS, el
    proceso recuperará aquellos en donde el BUSINESS_ID = 999999, para posteriormente actualizar este
    campo con el Business Id correspondiente a las columnas informadas como PARENT.

    Parámetros de entrada:

    Parámetros de salida:

    Notas:
    Autor: T32732
    Historia:
    Fecha        Autor                  Descripción
    ---------------------------------------------------
    2015-03-19   T32732                 Creacion de procedimiento
    ---------------------------------------------------
    2015-0819   T53605 (Pedro Dias)     Identacion y nomenclacion del SP para tener legibilidad en la lectura del codigo
                                        Modificacion del cursor que obtiene las ordenes extornadas, se obtiene el DOC_TYPE_CODE del extorno que determinará si
                                        la orden es importada o no ( code = 'IM').
                                        Solo se envian parametros obtenidos con datos del cursor de extornos, no se hardcodean codigos en este SP.
    ---------------------------------------------------

*********************************************************************************************/
PROCEDURE generate_bt_link_extornos_sp;

----------------------------------------------------------------------------------------------

/*********************************************************************************************
Nombre del programa:   retrieve_busn_trx_data_sp
    Sistema: ODS
    Objetivo: dado un business_id se cargan los registros de la business transaction correspondiente en una variable de
    tipo object collection que permitira luego ejecutar consultas rapetidas sobre el mismo conjunto de datos

    Parámetros de entrada:
    p_business_id: business_id del business transaction a cargar

    Parámetros de salida:
    p_busn_trx_data_table: object collection de types del object type ods_busn_trx_row_type.

    Notas:
    Autor: T52585
    Historia:
    Fecha        Autor        Descripción
    2013-04-03   T52585       Creacion de procedimiento

*********************************************************************************************/
PROCEDURE retrieve_busn_trx_data_sp (
        p_business_id           IN ods_business_transactions.business_id%TYPE,
        p_busn_trx_data_table   OUT ods_busn_trx_table_type
        );

--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   get_busn_trx_created_date_fn
    Sistema: ODS
    Objetivo: Criterio para considerar la fecha de inicio del negocio como "in loading process".
		El proceso manual toma  la diferencia entre la fecha actual y la fecha de creacion del business item
		Y el  proceso automatico toma como  fecha de partida la fecha en que llegó una cadena para ese
		business item. Técnicamente implica un cambio en la lógica de grabación del campo CREATED_DATE de la tabla de
		clasificación de los negocios PARA los registros insertados.

    Parámetros de entrada:
        p_busn_trx_data_table: Object Collection del Object Type ods_busn_trx_row_type. Contiene la cadena de
        business transactions pertenecientes a un Business_Id.



    Parámetros de salida:
        Salida de función:
            Retorna la fecha de creación del registro con menor orden (business_sequence menor)
            en donde el Portfolio_Type = '02'. Caso de no existir, busca el Portfolio_Type='90',
            y por último, de no existir los dos anteriores, busca el Portfolio_Type = '01'.

    Notas: SR ITTEN00264349
    Autor: T32732
    Historia:
    Fecha        Autor        Descripción
    ----------   -------      -----------------------
    30/09/2013  T32732       Creación de la Función


*********************************************************************************************/
FUNCTION get_busn_trx_created_date_fn( p_busn_trx_data_table	IN	ods_busn_trx_table_type
        )
RETURN ods_busn_tree_classifications.created_date%TYPE;
--------------------------------------------------------------------------------
--////////////////// PRIVATE MODULES IMPLEMENTATION ////////////////////////
PROCEDURE backup_busn_trx_sp(
        p_business_id    IN ods_business_doc_tree_ref_wrk.business_id%TYPE,
        p_busn_trx_bkp    IN OUT NOCOPY t_busn_trx_bkp_tab
        )
IS
    v_busn_trx_tab     t_busn_trx_tab;
        i PLS_INTEGER:= 0;
BEGIN
        -- recuperamos la cadena y la almacenamos en la tabla backup de cadenas
        get_row_busn_trx_sp(
                p_business_id    => p_business_id,
                p_busn_trx_tab    => v_busn_trx_tab
                );

    i:= NVL(p_busn_trx_bkp.LAST,0) + 1;
        IF v_busn_trx_tab.COUNT > 0
        THEN
                p_busn_trx_bkp(i):= v_busn_trx_tab;

                -- una vez almacenada la cadena borramos los registros de la cadena original
                remove_busn_trx_by_bid_sp(
                        p_business_id    => p_business_id
                        );

        END IF;

END backup_busn_trx_sp;

--------------------------------------------------------------------------------
PROCEDURE get_busn_trx_ref_doc_tree_sp(
        p_busn_trx_bkp        OUT t_busn_trx_bkp_tab
        )
IS
    CURSOR cur_busn_doc_tree_ref_wrk
        IS
        SELECT    business_id
        FROM    ods_business_doc_tree_ref_wrk
        ORDER BY business_id DESC;

    CURSOR cur_busn_id_ref_wrk
        IS
        SELECT    DISTINCT bt.business_id business_id
        FROM    ods_business_transactions bt
        WHERE    EXISTS (
                SELECT    1
                FROM    ods_business_doc_tree_wrk bd
                WHERE    bd.business_id = bt.business_reference_id
            )
        AND NOT EXISTS (
                SELECT    1
                FROM    ods_business_doc_tree_wrk bd
                WHERE    bd.business_id = bt.business_id
                );

        v_busn_doc_tree_ref_wrk_rec cur_busn_doc_tree_ref_wrk%ROWTYPE;

        v_timestamp DATE:= SYSDATE;
BEGIN
    -- damos de alta los registros en la tabla bid a procesar para que luego
        -- pueda ser recorrida por otros procesos
        FOR v_rec_busn_id_ref_wrk IN cur_busn_id_ref_wrk
        LOOP
        BEGIN
                        INSERT INTO ods_business_doc_tree_ref_wrk (
                                business_id,
                                created_by,
                                created_date
                                )
                        VALUES (
                                v_rec_busn_id_ref_wrk.business_id,
                                c_get_busn_trx_ref_process,
                                v_timestamp
                                );
        EXCEPTION
                    WHEN dup_val_on_index
                        THEN
                            UPDATE    ods_business_doc_tree_ref_wrk
                                SET    created_by = c_get_busn_trx_ref_process,
                                    created_date = v_timestamp
                                WHERE    business_id = v_rec_busn_id_ref_wrk.business_id;
        END;
        END LOOP;

        -- almacenamos las cadenas de las business id recuperadas
        FOR v_busn_doc_tree_ref_wrk_rec IN cur_busn_doc_tree_ref_wrk
        LOOP
                backup_busn_trx_sp(
                        p_business_id    => v_busn_doc_tree_ref_wrk_rec.business_id,
                        p_busn_trx_bkp    => p_busn_trx_bkp
                        );

        END LOOP;

END get_busn_trx_ref_doc_tree_sp;

--------------------------------------------------------------------------------

PROCEDURE remove_busn_trx_by_bid_sp(
        p_business_id    ods_business_transactions.business_id%TYPE
        )
IS
        CURSOR cur_delete_busn_trx(
                p_c_business_id ods_business_transactions.business_id%TYPE
                )
        IS
        SELECT    business_id,
            business_sequence
        FROM    ods_business_transactions
        WHERE    business_id = p_c_business_id
        ORDER BY business_sequence DESC;

        v_delete_busn_trx_rec cur_delete_busn_trx%ROWTYPE;
BEGIN
    FOR v_delete_busn_trx_rec IN cur_delete_busn_trx(
            p_c_business_id => p_business_id
                )
        LOOP
                DELETE    ods_business_transactions
                WHERE    business_id = v_delete_busn_trx_rec.business_id
                AND        business_sequence = v_delete_busn_trx_rec.business_sequence;
        END LOOP;

END remove_busn_trx_by_bid_sp;

--------------------------------------------------------------------------------

PROCEDURE remove_busn_trx_by_bid_ref_sp(
    p_business_id        ods_business_transactions.business_id%TYPE,
        p_business_reference_id    ods_business_transactions.business_reference_id%TYPE
        )
IS
BEGIN

    DELETE    ods_business_transactions
        WHERE    business_id = p_business_id
        AND    business_reference_id = p_business_reference_id;

END remove_busn_trx_by_bid_ref_sp;

--------------------------------------------------------------------------------

PROCEDURE insert_busn_trx_row_sp(
        p_insert_busn_trx_tab_row IN t_busn_trx_tab_row
        )
IS

        v_parent_doc_num        VARCHAR2 (10);
        v_doc_num    		VARCHAR2 (10);

BEGIN
        --verificamos que el document_number no se pase de los 8 digitos
        v_parent_doc_num:= p_insert_busn_trx_tab_row.parent_doc_number;

        IF    (LENGTH(p_insert_busn_trx_tab_row.parent_doc_number) = 10
          AND (p_insert_busn_trx_tab_row.parent_doc_portfolio_type in ('01', '40') OR p_insert_busn_trx_tab_row.doc_portfolio_type in ('01', '40'))
          AND p_insert_busn_trx_tab_row.data_source_system = 'ONE')
        THEN
                v_parent_doc_num := SUBSTR(v_parent_doc_num,3,8);
        END IF;

        v_doc_num:= p_insert_busn_trx_tab_row.document_number;

        IF    (LENGTH(p_insert_busn_trx_tab_row.document_number) = 10
          AND (p_insert_busn_trx_tab_row.parent_doc_portfolio_type in ('01', '40') OR p_insert_busn_trx_tab_row.doc_portfolio_type in ('01', '40'))
          AND p_insert_busn_trx_tab_row.data_source_system = 'ONE')
        THEN
                v_doc_num := SUBSTR(v_doc_num,3,8);
        END IF;

        INSERT INTO ods_business_transactions(
                business_id                ,
                business_sequence          ,
                business_reference_id      ,
                business_reference_seq     ,
                doc_portfolio_type         ,
                doc_comm_location_code     ,
                document_year              ,
                document_month             ,
                doc_type_code              ,
                document_number            ,
                document_item_num          ,
                document_split_num         ,
                unlinked_doc_flag          ,
                data_source_system         ,
                parent_doc_portfolio_type  ,
                parent_doc_comm_loc        ,
                parent_doc_year            ,
                parent_doc_month           ,
                parent_doc_type_code       ,
                parent_doc_number          ,
                parent_doc_item_num        ,
                parent_doc_split_num       ,
                historical_link_flag       ,
                secondary_costing_flag     ,
                origin_business_id         ,
                document_family_code       ,
                integration_type       ,
                created_date               ,
                created_by                 ,
                last_updated_date          ,
                last_updated_by            ,
                deleted_date               ,
                deleted_by
                )
        VALUES (
                p_insert_busn_trx_tab_row.business_id                ,
                p_insert_busn_trx_tab_row.business_sequence          ,
                p_insert_busn_trx_tab_row.business_reference_id      ,
                p_insert_busn_trx_tab_row.business_reference_seq     ,
                p_insert_busn_trx_tab_row.doc_portfolio_type         ,
                p_insert_busn_trx_tab_row.doc_comm_location_code     ,
                p_insert_busn_trx_tab_row.document_year              ,
                p_insert_busn_trx_tab_row.document_month             ,
                p_insert_busn_trx_tab_row.doc_type_code              ,
                v_doc_num            ,
                p_insert_busn_trx_tab_row.document_item_num          ,
                p_insert_busn_trx_tab_row.document_split_num         ,
                p_insert_busn_trx_tab_row.unlinked_doc_flag          ,
                p_insert_busn_trx_tab_row.data_source_system         ,
                p_insert_busn_trx_tab_row.parent_doc_portfolio_type  ,
                p_insert_busn_trx_tab_row.parent_doc_comm_loc        ,
                p_insert_busn_trx_tab_row.parent_doc_year            ,
                p_insert_busn_trx_tab_row.parent_doc_month           ,
                p_insert_busn_trx_tab_row.parent_doc_type_code       ,
                v_parent_doc_num          ,
                p_insert_busn_trx_tab_row.parent_doc_item_num        ,
                p_insert_busn_trx_tab_row.parent_doc_split_num       ,
                p_insert_busn_trx_tab_row.historical_link_flag       ,
                p_insert_busn_trx_tab_row.secondary_costing_flag     ,
                p_insert_busn_trx_tab_row.origin_business_id         ,
                p_insert_busn_trx_tab_row.document_family_code       ,
                p_insert_busn_trx_tab_row.integration_type           ,
                p_insert_busn_trx_tab_row.created_date               ,
                p_insert_busn_trx_tab_row.created_by                 ,
                p_insert_busn_trx_tab_row.last_updated_date          ,
                p_insert_busn_trx_tab_row.last_updated_by            ,
                p_insert_busn_trx_tab_row.deleted_date               ,
                p_insert_busn_trx_tab_row.deleted_by
                );

EXCEPTION
        WHEN dup_val_on_index
        THEN
                -- registramos los datos del nodo
                ods_busn_doc_tree_error_pkg.add_error_sp(
                        p_business_id                   => p_insert_busn_trx_tab_row.business_id,
                        p_parent_doc_comm_loc_code      => p_insert_busn_trx_tab_row.parent_doc_comm_loc,
                        p_parent_doc_portfolio_type     => p_insert_busn_trx_tab_row.parent_doc_portfolio_type,
                        p_parent_doc_year               => p_insert_busn_trx_tab_row.parent_doc_year,
                        p_parent_doc_month              => LPAD(TRIM(p_insert_busn_trx_tab_row.parent_doc_month),c_doc_month_length,'0'),
                        p_parent_doc_type_code          => p_insert_busn_trx_tab_row.parent_doc_type_code,
                        p_parent_doc_num                => LPAD(TRIM(p_insert_busn_trx_tab_row.parent_doc_number),c_doc_num_length,'0'),
                        p_parent_doc_item_num           => p_insert_busn_trx_tab_row.parent_doc_item_num,
                        p_parent_doc_split_num          => p_insert_busn_trx_tab_row.parent_doc_split_num,
                        p_error_desc                    => c_doc_tree_twice_ins_gen_desc,
                        p_error_generation_date         => SYSDATE,
                        p_process_name                  => c_replace_busn_trx_process
                        );

                RAISE e_doc_tree_node_twice_insert;
END insert_busn_trx_row_sp;
--------------------------------------------------------------------------------

PROCEDURE insert_busn_trx_row_sp(
        p_busn_trx_tab IN t_busn_trx_tab
        )
IS
        i PLS_INTEGER;
BEGIN
        i := p_busn_trx_tab.FIRST;
        WHILE i IS NOT NULL
        LOOP
                insert_busn_trx_row_sp(
                p_insert_busn_trx_tab_row => p_busn_trx_tab(i)
                );

            i := p_busn_trx_tab.NEXT(i);
        END LOOP;
END insert_busn_trx_row_sp;

--------------------------------------------------------------------------------

PROCEDURE add_busn_trx_root_record_sp(
        p_busn_doc_tree_tab_row IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_busn_sequence         IN s_busn_sequence,
        p_process_name          IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date          IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        )
IS

        v_secondary_costing_flag        ods_business_transactions.secondary_costing_flag%TYPE;
	v_parent_doc_num                VARCHAR2 (10);

BEGIN
        -- seteamos el flag se secondary costing flag
        IF p_busn_doc_tree_tab_row.data_source_system = ods_common_pkg.c_ten_data_source
        THEN
                v_secondary_costing_flag:= p_busn_doc_tree_tab_row.secondary_costing_flag;
        ELSE
                v_secondary_costing_flag:= ods_common_pkg.c_str_no;
        END IF;

        --verificamos que el document_number no se pase de los 8 digitos
        v_parent_doc_num:= p_busn_doc_tree_tab_row.parent_doc_num;

        IF    (LENGTH(p_busn_doc_tree_tab_row.parent_doc_num) = 10
          AND (p_busn_doc_tree_tab_row.parent_doc_portfolio_type in ('01', '40') OR p_busn_doc_tree_tab_row.doc_portfolio_type in ('01', '40'))
          AND p_busn_doc_tree_tab_row.data_source_system = 'ONE')
        THEN
                v_parent_doc_num := SUBSTR(v_parent_doc_num,3,8);
        END IF;

        INSERT INTO ods_business_transactions (
                business_id,
                business_sequence,
                business_reference_id,
                business_reference_seq,
                doc_portfolio_type,
                doc_comm_location_code,
                document_year,
                document_month,
                doc_type_code,
                document_number,
                document_item_num,
                document_split_num,
                parent_doc_portfolio_type,
                parent_doc_comm_loc,
                parent_doc_year,
                parent_doc_month,
                parent_doc_type_code,
                parent_doc_number,
                parent_doc_item_num,
                parent_doc_split_num,
                data_source_system,
                historical_link_flag,
                secondary_costing_flag,
                origin_business_id,
                document_family_code,
                integration_type,
                created_date,
                created_by,
                last_updated_date,
                last_updated_by,
                deleted_date,
                deleted_by
                )
        VALUES (
                p_busn_doc_tree_tab_row.business_id,
                p_busn_sequence,
                NULL,
                NULL,
                p_busn_doc_tree_tab_row.parent_doc_portfolio_type,
                p_busn_doc_tree_tab_row.parent_doc_comm_loc_code,
                p_busn_doc_tree_tab_row.parent_doc_year,
                LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0'),
                p_busn_doc_tree_tab_row.parent_doc_type_code,
                LPAD(TRIM(v_parent_doc_num),c_doc_num_length,'0'),
                p_busn_doc_tree_tab_row.parent_doc_item_num,
                p_busn_doc_tree_tab_row.parent_doc_split_num,
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
                p_busn_doc_tree_tab_row.data_source_system,
                p_busn_doc_tree_tab_row.historical_link_flag,
                v_secondary_costing_flag,
                NULL,
                p_busn_doc_tree_tab_row.parent_family_code,
                p_busn_doc_tree_tab_row.integration_type,
                p_process_date,
                p_process_name,
                p_process_date,
                p_process_name,
                NULL,
                NULL
                );
EXCEPTION
        WHEN dup_val_on_index
        THEN
                -- registramos los datos del nodo
                ods_busn_doc_tree_error_pkg.add_error_sp(
                        p_business_id                   => p_busn_doc_tree_tab_row.business_id,
                        p_parent_doc_comm_loc_code      => p_busn_doc_tree_tab_row.parent_doc_comm_loc_code,
                        p_parent_doc_portfolio_type     => p_busn_doc_tree_tab_row.parent_doc_portfolio_type,
                        p_parent_doc_year               => p_busn_doc_tree_tab_row.parent_doc_year,
                        p_parent_doc_month              => LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0'),
                        p_parent_doc_type_code          => p_busn_doc_tree_tab_row.parent_doc_type_code,
                        p_parent_doc_num                => LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_num),c_doc_num_length,'0'),
                        p_parent_doc_item_num           => p_busn_doc_tree_tab_row.parent_doc_item_num,
                        p_parent_doc_split_num          => p_busn_doc_tree_tab_row.parent_doc_split_num,
                        p_error_desc                    => c_doc_tree_twice_ins_root_desc,
                        p_error_generation_date         => SYSDATE,
                        p_process_name                  => c_replace_busn_trx_process
                        );

        RAISE e_doc_tree_node_twice_insert;
END add_busn_trx_root_record_sp;

------------------------------------------------------------------------------------------------

PROCEDURE add_busn_trx_ack_record_sp(
        p_busn_doc_tree_tab_row         IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_busn_sequence                 IN s_busn_sequence,
        p_mill_ack_portfolio_type       IN ods_business_document_tree.doc_portfolio_type%TYPE,
        p_process_name                  IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date                  IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        )
IS

        v_secondary_costing_flag    ods_business_transactions.secondary_costing_flag%TYPE;
        v_parent_doc_num            VARCHAR2 (10);
        v_doc_num                   VARCHAR2 (10);

BEGIN
        -- seteamos el flag se secondary costing flag
        IF p_busn_doc_tree_tab_row.data_source_system = ods_common_pkg.c_ten_data_source
        THEN
                v_secondary_costing_flag:= p_busn_doc_tree_tab_row.secondary_costing_flag;
        ELSE
                v_secondary_costing_flag:= ods_common_pkg.c_str_no;
        END IF;

        --verificamos que el document_number no se pase de los 8 digitos
        v_parent_doc_num:= p_busn_doc_tree_tab_row.parent_doc_num;

        IF    (LENGTH(p_busn_doc_tree_tab_row.parent_doc_num) = 10
          AND (p_busn_doc_tree_tab_row.parent_doc_portfolio_type in ('01', '40') OR p_busn_doc_tree_tab_row.doc_portfolio_type in ('01', '40'))
          AND p_busn_doc_tree_tab_row.data_source_system = 'ONE')
        THEN
                v_parent_doc_num := SUBSTR(v_parent_doc_num,3,8);
        END IF;

        v_doc_num := p_busn_doc_tree_tab_row.mill_ack_doc_num;

        IF    (LENGTH(p_busn_doc_tree_tab_row.mill_ack_doc_num) = 10
          AND (p_busn_doc_tree_tab_row.parent_doc_portfolio_type in ('01', '40') OR p_busn_doc_tree_tab_row.doc_portfolio_type in ('01', '40'))
          AND p_busn_doc_tree_tab_row.data_source_system = 'ONE')
        THEN
                v_doc_num := SUBSTR(v_doc_num,3,8);
        END IF;

        INSERT INTO ods_business_transactions (
                business_id,
                business_sequence,
                business_reference_id,
                business_reference_seq,
                doc_portfolio_type,
                doc_comm_location_code,
                document_year,
                document_month,
                doc_type_code,
                document_number,
                document_item_num,
                document_split_num,
                parent_doc_portfolio_type,
                parent_doc_comm_loc,
                parent_doc_year,
                parent_doc_month,
                parent_doc_type_code,
                parent_doc_number,
                parent_doc_item_num,
                parent_doc_split_num,
                data_source_system,
                historical_link_flag,
                secondary_costing_flag,
                origin_business_id,
                document_family_code,
                integration_type,
                created_date,
                created_by,
                last_updated_date,
                last_updated_by,
                deleted_date,
                deleted_by
                )
        VALUES (
                p_busn_doc_tree_tab_row.business_id,
                p_busn_sequence,
                NULL,
                NULL,
                p_mill_ack_portfolio_type,
                p_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code,
                p_busn_doc_tree_tab_row.mill_ack_doc_year,
                LPAD(TRIM(p_busn_doc_tree_tab_row.mill_ack_doc_month),c_doc_month_length,'0'),
                NVL(p_busn_doc_tree_tab_row.mill_ack_doc_type_code,c_null_ack_doc_type),
                LPAD(TRIM(v_doc_num),c_doc_num_length,'0'),
                p_busn_doc_tree_tab_row.mill_ack_item_num,
                --
                --1,                    -- como el conferma no tiene split number se le asigna 1
                -- se incorpora logica en la carga de parent_doc_porfolio_type
                p_busn_doc_tree_tab_row.mill_ack_split_num,
                p_busn_doc_tree_tab_row.parent_doc_portfolio_type,
                p_busn_doc_tree_tab_row.parent_doc_comm_loc_code,
                p_busn_doc_tree_tab_row.parent_doc_year,
                LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0'),
                p_busn_doc_tree_tab_row.parent_doc_type_code,
                LPAD(TRIM(v_parent_doc_num),c_doc_num_length,'0'),
                p_busn_doc_tree_tab_row.parent_doc_item_num,
                p_busn_doc_tree_tab_row.parent_doc_split_num,
                p_busn_doc_tree_tab_row.data_source_system,
                p_busn_doc_tree_tab_row.historical_link_flag,
                v_secondary_costing_flag,
                NULL,
                --
                --NULL,
                --p_busn_doc_tree_tab_row.mill_ack_family_code,
                p_busn_doc_tree_tab_row.doc_family_code,
                --
                p_busn_doc_tree_tab_row.integration_type,
                p_process_date,
                p_process_name,
                p_process_date,
                p_process_name,
                NULL,
                NULL
        );
EXCEPTION
        WHEN dup_val_on_index
        THEN
                -- registramos los datos del nodo
                ods_busn_doc_tree_error_pkg.add_error_sp(
                        p_business_id                   => p_busn_doc_tree_tab_row.business_id,
                        p_parent_doc_comm_loc_code      => p_busn_doc_tree_tab_row.parent_doc_comm_loc_code,
                        p_parent_doc_portfolio_type     => p_busn_doc_tree_tab_row.parent_doc_portfolio_type,
                        p_parent_doc_year               => p_busn_doc_tree_tab_row.parent_doc_year,
                        p_parent_doc_month              => LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0'),
                        p_parent_doc_type_code          => p_busn_doc_tree_tab_row.parent_doc_type_code,
                        p_parent_doc_num                => LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_num),c_doc_num_length,'0'),
                        p_parent_doc_item_num           => p_busn_doc_tree_tab_row.parent_doc_item_num,
                        p_parent_doc_split_num          => p_busn_doc_tree_tab_row.parent_doc_split_num,
                        p_error_desc                    => c_doc_tree_twice_ins_ack_desc,
                        p_error_generation_date         => SYSDATE,
                        p_process_name                  => c_replace_busn_trx_process
                        );

        RAISE e_doc_tree_node_twice_insert;
END add_busn_trx_ack_record_sp;

------------------------------------------------------------------------------------------------

PROCEDURE add_busn_trx_add_ack_record_sp(
        p_busn_doc_tree_tab_row         IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_busn_sequence                 IN s_busn_sequence,
        p_mill_ack_portfolio_type       IN ods_business_document_tree.doc_portfolio_type%TYPE,
        p_process_name                  IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date                  IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        )
IS

        v_secondary_costing_flag        ods_business_transactions.secondary_costing_flag%TYPE;
        v_parent_doc_num                VARCHAR2 (10);
        v_doc_num    			VARCHAR2 (10);

BEGIN
        -- seteamos el flag se secondary costing flag
        IF p_busn_doc_tree_tab_row.data_source_system = ods_common_pkg.c_ten_data_source
        THEN
                v_secondary_costing_flag:= p_busn_doc_tree_tab_row.secondary_costing_flag;
        ELSE
                v_secondary_costing_flag:= ods_common_pkg.c_str_no;
        END IF;

       --verificamos que el document_number no se pase de los 8 digitos

        v_parent_doc_num:= p_busn_doc_tree_tab_row.parent_doc_num;

        IF    (LENGTH(p_busn_doc_tree_tab_row.parent_doc_num) = 10
          AND (p_busn_doc_tree_tab_row.parent_doc_portfolio_type in ('01', '40') OR p_busn_doc_tree_tab_row.doc_portfolio_type in ('01', '40'))
          AND p_busn_doc_tree_tab_row.data_source_system = 'ONE')
        THEN
                v_parent_doc_num := SUBSTR(v_parent_doc_num,3,8);
        END IF;

        v_doc_num:= p_busn_doc_tree_tab_row.mill_ack_doc_num;

        IF    (LENGTH(p_busn_doc_tree_tab_row.mill_ack_doc_num) = 10
          AND (p_busn_doc_tree_tab_row.parent_doc_portfolio_type in ('01', '40') OR p_busn_doc_tree_tab_row.doc_portfolio_type in ('01', '40'))
          AND p_busn_doc_tree_tab_row.data_source_system = 'ONE')
        THEN
                v_doc_num := SUBSTR(v_doc_num,3,8);
        END IF;

        INSERT INTO ods_business_transactions (
                business_id,
                business_sequence,
                business_reference_id,
                business_reference_seq,
                doc_portfolio_type,
                doc_comm_location_code,
                document_year,
                document_month,
                doc_type_code,
                document_number,
                document_item_num,
                document_split_num,
                parent_doc_portfolio_type,
                parent_doc_comm_loc,
                parent_doc_year,
                parent_doc_month,
                parent_doc_type_code,
                parent_doc_number,
                parent_doc_item_num,
                parent_doc_split_num,
                data_source_system,
                historical_link_flag,
                secondary_costing_flag,
                origin_business_id,
                document_family_code,
                integration_type,
                created_date,
                created_by,
                last_updated_date,
                last_updated_by,
                deleted_date,
                deleted_by
                )
        VALUES (
                p_busn_doc_tree_tab_row.business_id,
                p_busn_sequence,
                NULL,
                NULL,
                c_mill_ack_portfolio_type,
                p_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code,
                p_busn_doc_tree_tab_row.mill_ack_doc_year,
                LPAD(TRIM(p_busn_doc_tree_tab_row.mill_ack_doc_month),c_doc_month_length,'0'),
                NVL(p_busn_doc_tree_tab_row.mill_ack_doc_type_code,c_null_ack_doc_type),
                LPAD(TRIM(v_doc_num),c_doc_num_length,'0'),
                p_busn_doc_tree_tab_row.mill_ack_item_num,
                --1,                    -- como el conferma no tiene split number se le asigna 1
                -- Se incorpora logica en la carga del mill_ack_split_num
                p_busn_doc_tree_tab_row.mill_ack_split_num,
                -- se incorpora logica en la carga de parent_doc_porfolio_type
                NVL(p_mill_ack_portfolio_type,p_busn_doc_tree_tab_row.doc_portfolio_type),
                p_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code,
                p_busn_doc_tree_tab_row.mill_ack_doc_year,
                LPAD(TRIM(p_busn_doc_tree_tab_row.mill_ack_doc_month),c_doc_month_length,'0'),
                --se carga el parent_doc_type_code con el valor del parametro en lugar de p_busn_doc_tree_tab_row.mill_ack_doc_type_code
                NVL(p_busn_doc_tree_tab_row.mill_ack_doc_type_code,c_null_ack_doc_type),
                LPAD(TRIM(v_parent_doc_num),c_doc_num_length,'0'),
                p_busn_doc_tree_tab_row.mill_ack_item_num,
                1,                    -- como el conferma no tiene split number se le asigna 1
                p_busn_doc_tree_tab_row.data_source_system,
                p_busn_doc_tree_tab_row.historical_link_flag,
                v_secondary_costing_flag,
                NULL,
                --
                --NULL,
                --p_busn_doc_tree_tab_row.mill_ack_family_code,
                p_busn_doc_tree_tab_row.doc_family_code,
                --
                p_busn_doc_tree_tab_row.integration_type,
                p_process_date,
                p_process_name,
                p_process_date,
                p_process_name,
                NULL,
                NULL
        );
EXCEPTION
        WHEN dup_val_on_index
        THEN
                -- registramos los datos del nodo
                ods_busn_doc_tree_error_pkg.add_error_sp(
                        p_business_id                   => p_busn_doc_tree_tab_row.business_id,
                        p_parent_doc_comm_loc_code      => p_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code,
                        p_parent_doc_portfolio_type     => p_busn_doc_tree_tab_row.doc_portfolio_type,
                        p_parent_doc_year               => p_busn_doc_tree_tab_row.mill_ack_doc_year,
                        p_parent_doc_month              => LPAD(TRIM(p_busn_doc_tree_tab_row.mill_ack_doc_month),c_doc_month_length,'0'),
                        p_parent_doc_type_code          => p_busn_doc_tree_tab_row.mill_ack_doc_type_code,
                        p_parent_doc_num                => LPAD(TRIM(p_busn_doc_tree_tab_row.mill_ack_doc_num),c_doc_num_length,'0'),
                        p_parent_doc_item_num           => p_busn_doc_tree_tab_row.mill_ack_item_num,
                        p_parent_doc_split_num          => p_busn_doc_tree_tab_row.mill_ack_split_num,
                        p_error_desc                    => c_doc_tree_twice_ins_ack_desc,
                        p_error_generation_date         => SYSDATE,
                        p_process_name                  => c_replace_busn_trx_process
                        );

        RAISE e_doc_tree_node_twice_insert;
END add_busn_trx_add_ack_record_sp;
------------------------------------------------------------------------------------------------

PROCEDURE add_busn_trx_child_record_sp(
        p_busn_doc_tree_tab_row IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_busn_sequence         IN s_busn_sequence,
        p_process_name          IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date          IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        )
IS

        v_secondary_costing_flag        ods_business_transactions.secondary_costing_flag%TYPE;
        v_parent_doc_num                VARCHAR2 (10);
        v_doc_num    		        VARCHAR2 (10);

BEGIN
        -- seteamos el flag se secondary costing flag
        IF p_busn_doc_tree_tab_row.data_source_system = ods_common_pkg.c_ten_data_source
        THEN
                v_secondary_costing_flag:= p_busn_doc_tree_tab_row.secondary_costing_flag;
        ELSE
                v_secondary_costing_flag:= ods_common_pkg.c_str_no;
        END IF;

       --verificamos que el document_number no se pase de los 8 digitos
        v_parent_doc_num:= p_busn_doc_tree_tab_row.parent_doc_num;

        IF    (LENGTH(p_busn_doc_tree_tab_row.parent_doc_num) = 10
          AND (p_busn_doc_tree_tab_row.parent_doc_portfolio_type in ('01', '40') OR p_busn_doc_tree_tab_row.doc_portfolio_type in ('01', '40'))
          AND p_busn_doc_tree_tab_row.data_source_system = 'ONE')
        THEN
                v_parent_doc_num := SUBSTR(v_parent_doc_num,3,8);
        END IF;

        v_doc_num := p_busn_doc_tree_tab_row.doc_num;

        IF    (LENGTH(p_busn_doc_tree_tab_row.doc_num) = 10
          AND (p_busn_doc_tree_tab_row.parent_doc_portfolio_type in ('01', '40') OR p_busn_doc_tree_tab_row.doc_portfolio_type in ('01', '40'))
          AND p_busn_doc_tree_tab_row.data_source_system = 'ONE')
        THEN
                v_doc_num := SUBSTR(v_doc_num,3,8);
        END IF;

        INSERT INTO ods_business_transactions (
                business_id,
                business_sequence,
                business_reference_id,
                business_reference_seq,
                doc_portfolio_type,
                doc_comm_location_code,
                document_year,
                document_month,
                doc_type_code,
                document_number,
                document_item_num,
                document_split_num,
                parent_doc_portfolio_type,
                parent_doc_comm_loc,
                parent_doc_year,
                parent_doc_month,
                parent_doc_type_code,
                parent_doc_number,
                parent_doc_item_num,
                parent_doc_split_num,
                data_source_system,
                historical_link_flag,
                secondary_costing_flag,
                origin_business_id,
                document_family_code,
                integration_type,
                created_date,
                created_by,
                last_updated_date,
                last_updated_by,
                deleted_date,
                deleted_by
                )
        VALUES (
                p_busn_doc_tree_tab_row.business_id,
                p_busn_sequence,
                NULL,
                NULL,
                p_busn_doc_tree_tab_row.doc_portfolio_type,
                p_busn_doc_tree_tab_row.doc_comm_loc_code,
                p_busn_doc_tree_tab_row.doc_year,
                LPAD(TRIM(p_busn_doc_tree_tab_row.doc_month),c_doc_month_length,'0'),
                p_busn_doc_tree_tab_row.doc_type_code,
                LPAD(TRIM(v_doc_num),c_doc_num_length,'0'),
                p_busn_doc_tree_tab_row.doc_item_num,
                p_busn_doc_tree_tab_row.doc_split_num,
                p_busn_doc_tree_tab_row.parent_doc_portfolio_type,
                p_busn_doc_tree_tab_row.parent_doc_comm_loc_code,
                p_busn_doc_tree_tab_row.parent_doc_year,
                LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0'),
                p_busn_doc_tree_tab_row.parent_doc_type_code,
                LPAD(TRIM(v_parent_doc_num),c_doc_num_length,'0'),
                p_busn_doc_tree_tab_row.parent_doc_item_num,
                p_busn_doc_tree_tab_row.parent_doc_split_num,
                p_busn_doc_tree_tab_row.data_source_system,
                p_busn_doc_tree_tab_row.historical_link_flag,
                v_secondary_costing_flag,
                NULL,
                --
                p_busn_doc_tree_tab_row.doc_family_code,
                --NVL(p_busn_doc_tree_tab_row.doc_family_code, p_busn_doc_tree_tab_row.parent_family_code),
                --
                p_busn_doc_tree_tab_row.integration_type,
                p_process_date,
                p_process_name,
                p_process_date,
                p_process_name,
                NULL,
                NULL
                );
EXCEPTION
        WHEN dup_val_on_index
        THEN
                -- registramos los datos del nodo
                ods_busn_doc_tree_error_pkg.add_error_sp(
                        p_business_id                   => p_busn_doc_tree_tab_row.business_id,
                        p_parent_doc_comm_loc_code      => p_busn_doc_tree_tab_row.parent_doc_comm_loc_code,
                        p_parent_doc_portfolio_type     => p_busn_doc_tree_tab_row.parent_doc_portfolio_type,
                        p_parent_doc_year               => p_busn_doc_tree_tab_row.parent_doc_year,
                        p_parent_doc_month              => LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0'),
                        p_parent_doc_type_code          => p_busn_doc_tree_tab_row.parent_doc_type_code,
                        p_parent_doc_num                => LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_num),c_doc_num_length,'0'),
                        p_parent_doc_item_num           => p_busn_doc_tree_tab_row.parent_doc_item_num,
                        p_parent_doc_split_num          => p_busn_doc_tree_tab_row.parent_doc_split_num,
                        p_error_desc                    => c_doc_tree_twice_ins_chld_desc,
                        p_error_generation_date         => SYSDATE,
                        p_process_name                  => c_replace_busn_trx_process
                        );

        RAISE e_doc_tree_node_twice_insert;
END add_busn_trx_child_record_sp;

------------------------------------------------------------------------------------------------

PROCEDURE add_build_stk_busn_trx_seq_sp(
        p_busn_doc_tree_tab_row IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_busn_sequence         IN OUT s_busn_sequence,
        p_process_name          IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date          IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        )
IS
        v_ref_busn_trx_business_id      ods_business_transactions.business_id%TYPE;
        v_ref_busn_trx_business_seq     ods_business_transactions.business_sequence%TYPE;

        cur_busn_trx                    ods_business_transaction_pkg.t_refcursor;
        v_busn_trx_rec                  t_busn_trx_tab_row;

        v_parent_doc_comm_loc           ods_business_transactions.parent_doc_comm_loc%TYPE;
        v_parent_doc_year               ods_business_transactions.parent_doc_year%TYPE;
        v_parent_doc_portfolio_type     ods_business_transactions.parent_doc_portfolio_type%TYPE;
        v_parent_doc_month              ods_business_transactions.parent_doc_month%TYPE;
        v_parent_doc_type_code          ods_business_transactions.parent_doc_type_code%TYPE;
        v_parent_doc_number             ods_business_transactions.parent_doc_number%TYPE;
        v_parent_doc_item_num           ods_business_transactions.parent_doc_item_num%TYPE;
        v_parent_doc_split_num          ods_business_transactions.parent_doc_split_num%TYPE;

        v_reprocess_qty                 ods_business_doc_tree_wrk_err.reprocess_qty%TYPE;
        v_branch_historical_link_flag   ods_business_document_tree.historical_link_flag%TYPE;
BEGIN
        -- si se trata del primer nodo del document tree se genera el nodo padre de business transaction
        IF p_busn_doc_tree_tab_row.doc_node_level = 1
        THEN

                v_busn_trx_rec.business_id              := p_busn_doc_tree_tab_row.business_id;
                v_busn_trx_rec.business_sequence        := p_busn_sequence;
                v_busn_trx_rec.doc_comm_location_code   := p_busn_doc_tree_tab_row.parent_doc_comm_loc_code;
                v_busn_trx_rec.document_year            := p_busn_doc_tree_tab_row.parent_doc_year;
                v_busn_trx_rec.doc_portfolio_type       := p_busn_doc_tree_tab_row.parent_doc_portfolio_type;
                v_busn_trx_rec.document_month           := LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0');
                v_busn_trx_rec.doc_type_code            := p_busn_doc_tree_tab_row.parent_doc_type_code;
                v_busn_trx_rec.document_number          := LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_num),c_doc_num_length,'0');
                v_busn_trx_rec.document_item_num        := p_busn_doc_tree_tab_row.parent_doc_item_num;
                v_busn_trx_rec.document_split_num       := p_busn_doc_tree_tab_row.parent_doc_split_num;
                v_busn_trx_rec.document_family_code     := p_busn_doc_tree_tab_row.parent_family_code;
                v_busn_trx_rec.historical_link_flag     := p_busn_doc_tree_tab_row.historical_link_flag;
                v_busn_trx_rec.secondary_costing_flag   := p_busn_doc_tree_tab_row.secondary_costing_flag;
                v_busn_trx_rec.data_source_system       := p_busn_doc_tree_tab_row.data_source_system;
                v_busn_trx_rec.unlinked_doc_flag        := ods_common_pkg.c_str_no;
                v_busn_trx_rec.integration_type         := p_busn_doc_tree_tab_row.integration_type;
                v_busn_trx_rec.created_date             := p_process_date;
                v_busn_trx_rec.created_by               := p_process_name;
                v_busn_trx_rec.last_updated_date        := p_process_date;
                v_busn_trx_rec.last_updated_by          := p_process_name;
                v_busn_trx_rec.deleted_date             := NULL;
                v_busn_trx_rec.deleted_by               := NULL;

                -- damos de alta el nodo padre
                insert_busn_trx_row_sp(
                        p_insert_busn_trx_tab_row    => v_busn_trx_rec
                        );

                -- incremenetamos la secuencia para el nodo nuevo
                p_busn_sequence:= p_busn_sequence + 1;

        END IF;
        -- se setea el  hisotrical link heredado de la cadena desde la cual se hace referencia y se asigna el
        -- mismo a todos nodos
        v_branch_historical_link_flag:= p_busn_doc_tree_tab_row.historical_link_flag;
        -- verificamos la existencia de un business transaction correspondiente al nodo del arbol
        -- completamos con ceros a la izquierda el doc_num del nodo del arbol para igualarlo al de busness trx
        get_busn_id_sp(
                p_doc_comm_location_code        => p_busn_doc_tree_tab_row.doc_comm_loc_code,
                p_document_year                 => p_busn_doc_tree_tab_row.doc_year,
                p_document_month                => LPAD(TRIM(p_busn_doc_tree_tab_row.doc_month),c_doc_month_length,'0'),
                p_doc_type_code                 => p_busn_doc_tree_tab_row.doc_type_code,
                p_document_number               => LPAD(TRIM(p_busn_doc_tree_tab_row.doc_num),c_doc_num_length,'0'),
                p_document_item_num             => p_busn_doc_tree_tab_row.doc_item_num,
                p_document_split_num            => p_busn_doc_tree_tab_row.doc_split_num,
                p_business_id                   => v_ref_busn_trx_business_id,
                p_business_sequence             => v_ref_busn_trx_business_seq
                );

        -- si se encuentra un registro , recuperamos la secuencia completa de transacciones
        IF v_ref_busn_trx_business_id IS NOT NULL
        THEN

                -- recuperamos la cadena
                get_row_busn_trx_sp(
                        p_business_id           => v_ref_busn_trx_business_id,
                        p_business_sequence     => v_ref_busn_trx_business_seq,
                        p_cur_busn_trx          => cur_busn_trx
                        );

                -- asignamos los valores del registro parent a partir de los datos del nodo de document tree
                v_parent_doc_comm_loc           := p_busn_doc_tree_tab_row.parent_doc_comm_loc_code;
                v_parent_doc_year               := p_busn_doc_tree_tab_row.parent_doc_year;
                v_parent_doc_portfolio_type     := p_busn_doc_tree_tab_row.parent_doc_portfolio_type;
                v_parent_doc_month              := LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0');
                v_parent_doc_type_code          := p_busn_doc_tree_tab_row.parent_doc_type_code;
                v_parent_doc_number             := LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_num),c_doc_num_length,'0');
                v_parent_doc_item_num           := p_busn_doc_tree_tab_row.parent_doc_item_num;
                v_parent_doc_split_num          := p_busn_doc_tree_tab_row.parent_doc_split_num;

                -- recorremos el cursor dando de alta registros en business transaction
                FETCH cur_busn_trx INTO v_busn_trx_rec;
                LOOP
                        EXIT WHEN cur_busn_trx%NOTFOUND;

                        -- modificamos los datos que cambian
                        -- NOTA: los campos doc_portfolio_type, doc_comm_location_code,document_year,document_month,doc_type_code,
                        -- document_number,document_item_num,document_split_num y data_source_system se pasan al cual se recuperan del cursor
                        v_busn_trx_rec.business_reference_id    := v_busn_trx_rec.business_id;
                        v_busn_trx_rec.business_reference_seq   := v_busn_trx_rec.business_sequence;
                        v_busn_trx_rec.business_id              := p_busn_doc_tree_tab_row.business_id;
                        v_busn_trx_rec.business_sequence        := p_busn_sequence;
                        v_busn_trx_rec.parent_doc_comm_loc      := v_parent_doc_comm_loc;
                        v_busn_trx_rec.parent_doc_year          := v_parent_doc_year;
                        v_busn_trx_rec.parent_doc_portfolio_type:= v_parent_doc_portfolio_type;
                        v_busn_trx_rec.parent_doc_month         := v_parent_doc_month;
                        v_busn_trx_rec.parent_doc_type_code     := v_parent_doc_type_code;
                        v_busn_trx_rec.parent_doc_number        := v_parent_doc_number;
                        v_busn_trx_rec.parent_doc_item_num      := v_parent_doc_item_num;
                        v_busn_trx_rec.parent_doc_split_num     := v_parent_doc_split_num;
                        v_busn_trx_rec.historical_link_flag     := v_branch_historical_link_flag;
                        v_busn_trx_rec.secondary_costing_flag   := v_busn_trx_rec.secondary_costing_flag;
                        v_busn_trx_rec.created_date             := p_process_date;
                        v_busn_trx_rec.created_by               := p_process_name;
                        v_busn_trx_rec.last_updated_date        := p_process_date;
                        v_busn_trx_rec.last_updated_by          := p_process_name;
                        v_busn_trx_rec.deleted_date             := NULL;
                        v_busn_trx_rec.deleted_by               := NULL;

                        -- damos de alta el nuevo registro
                        insert_busn_trx_row_sp(
                                p_insert_busn_trx_tab_row    => v_busn_trx_rec
                                );

                        -- incremenetamos la secuencia para el nodo nuevo
                        p_busn_sequence:= p_busn_sequence + 1;

                        -- almacenamos los datos del proximo registro parent
                        v_parent_doc_comm_loc        := v_busn_trx_rec.doc_comm_location_code;
                        v_parent_doc_year            := v_busn_trx_rec.document_year;
                        v_parent_doc_portfolio_type        := v_busn_trx_rec.doc_portfolio_type;
                        v_parent_doc_month            := v_busn_trx_rec.document_month;
                        v_parent_doc_type_code            := v_busn_trx_rec.doc_type_code;
                        v_parent_doc_number            := v_busn_trx_rec.document_number;
                        v_parent_doc_item_num            := v_busn_trx_rec.document_item_num;
                        v_parent_doc_split_num            := v_busn_trx_rec.document_split_num;

                        -- recuperamos el proximo registro de business transaction
                        FETCH  cur_busn_trx INTO v_busn_trx_rec;
                END LOOP;

                CLOSE cur_busn_trx;

                -- dado que encontamos la cadena referenciada ejecutamos el borrado de la business_id de la tabla de business_id
                -- sin referencia por si el mismo habia sido dado de alta en algun proceso anterior.
                ods_busn_doc_tree_error_pkg.delete_error_busn_id_sp(
                        p_business_id    => p_busn_doc_tree_tab_row.business_id
                        );

        ELSE
                -- si no cuentra la cadena referenciada se da de alta el nodo de la cadena original no encontrada
                -- como parte de otra cadena y luego se almacenan los datos para loguearlo posteriormente
                -- como un error.

                -- tambien se loguea el business id y se verifica si se debe eliminar de la tabla ods_business_doc_tree_wrk_err
                -- generamos el ultimo nodo de la cadena original (nodo de tipo link S)

                v_busn_trx_rec.business_id              := p_busn_doc_tree_tab_row.business_id;
                v_busn_trx_rec.business_sequence        := p_busn_sequence;
                v_busn_trx_rec.business_reference_id    := c_invalid_busn_trx_ref_id;
                v_busn_trx_rec.business_reference_seq   := c_invalid_busn_trx_ref_seq;
                v_busn_trx_rec.doc_comm_location_code   := p_busn_doc_tree_tab_row.doc_comm_loc_code;
                v_busn_trx_rec.document_year            := p_busn_doc_tree_tab_row.doc_year;
                v_busn_trx_rec.doc_portfolio_type       := p_busn_doc_tree_tab_row.doc_portfolio_type;
                v_busn_trx_rec.document_month           := LPAD(TRIM(p_busn_doc_tree_tab_row.doc_month),c_doc_month_length,'0');
                v_busn_trx_rec.doc_type_code            := p_busn_doc_tree_tab_row.doc_type_code;
                v_busn_trx_rec.document_number          := LPAD(TRIM(p_busn_doc_tree_tab_row.doc_num),c_doc_num_length,'0');
                v_busn_trx_rec.document_item_num        := p_busn_doc_tree_tab_row.doc_item_num;
                v_busn_trx_rec.document_split_num       := p_busn_doc_tree_tab_row.doc_split_num;
                v_busn_trx_rec.parent_doc_comm_loc      := p_busn_doc_tree_tab_row.parent_doc_comm_loc_code;
                v_busn_trx_rec.parent_doc_year          := p_busn_doc_tree_tab_row.parent_doc_year;
                v_busn_trx_rec.parent_doc_portfolio_type:= p_busn_doc_tree_tab_row.parent_doc_portfolio_type;
                v_busn_trx_rec.parent_doc_month         := LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0');
                v_busn_trx_rec.parent_doc_type_code     := p_busn_doc_tree_tab_row.parent_doc_type_code;
                v_busn_trx_rec.parent_doc_number        := LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_num),c_doc_num_length,'0');
                v_busn_trx_rec.parent_doc_item_num      := p_busn_doc_tree_tab_row.parent_doc_item_num;
                v_busn_trx_rec.parent_doc_split_num     := p_busn_doc_tree_tab_row.parent_doc_split_num;
                v_busn_trx_rec.data_source_system       := p_busn_doc_tree_tab_row.data_source_system;
                v_busn_trx_rec.historical_link_flag     := v_branch_historical_link_flag;
                v_busn_trx_rec.secondary_costing_flag   := p_busn_doc_tree_tab_row.secondary_costing_flag;
                --
                v_busn_trx_rec.document_family_code     := p_busn_doc_tree_tab_row.doc_family_code;
                --v_busn_trx_rec.document_family_code     := NVL(p_busn_doc_tree_tab_row.doc_family_code, p_busn_doc_tree_tab_row.parent_family_code);
                --
                v_busn_trx_rec.unlinked_doc_flag        := 'N';
                v_busn_trx_rec.integration_type         := p_busn_doc_tree_tab_row.integration_type;
                v_busn_trx_rec.created_date             := p_process_date;
                v_busn_trx_rec.created_by               := p_process_name;
                v_busn_trx_rec.last_updated_date        := p_process_date;
                v_busn_trx_rec.last_updated_by          := p_process_name;
                v_busn_trx_rec.deleted_date             := NULL;
                v_busn_trx_rec.deleted_by               := NULL;

                -- damos de alta el nodo
                insert_busn_trx_row_sp(
                        p_insert_busn_trx_tab_row    => v_busn_trx_rec
                        );

                -- incremenetamos la secuencia para el nodo nuevo
                p_busn_sequence:= p_busn_sequence + 1;

                -- registramos los datos del nodo padre del nodo que no se encuentra como referencia
                ods_busn_doc_tree_error_pkg.add_error_sp(
                        p_business_id                   => p_busn_doc_tree_tab_row.business_id,
                        p_parent_doc_comm_loc_code      => p_busn_doc_tree_tab_row.parent_doc_comm_loc_code,
                        p_parent_doc_portfolio_type     => p_busn_doc_tree_tab_row.parent_doc_portfolio_type,
                        p_parent_doc_year               => p_busn_doc_tree_tab_row.parent_doc_year,
                        p_parent_doc_month              => LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0'),
                        p_parent_doc_type_code          => p_busn_doc_tree_tab_row.parent_doc_type_code,
                        p_parent_doc_num                => LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_num),c_doc_num_length,'0'),
                        p_parent_doc_item_num           => p_busn_doc_tree_tab_row.parent_doc_item_num,
                        p_parent_doc_split_num          => p_busn_doc_tree_tab_row.parent_doc_split_num,
                        p_error_desc                    => c_busn_trx_ref_no_found_desc,
                        p_error_generation_date         => SYSDATE,
                        p_process_name                  => c_replace_busn_trx_process
                        );

                -- registramos el business id para reproceso
                ods_busn_doc_tree_error_pkg.record_error_busn_id_sp(
                        p_business_id   => p_busn_doc_tree_tab_row.business_id,
                        p_username      => c_replace_busn_trx_process,
                        p_reprocess_qty => v_reprocess_qty
                        );

                -- verificamos si debemos eliminar el business id de la tabla ods_business_doc_tree_wrk_err
                IF v_reprocess_qty = ods_busn_doc_tree_error_pkg.c_max_reprocess_qty
                THEN
                        -- eliminamos el registro
                        ods_busn_doc_tree_error_pkg.delete_error_busn_id_sp(
                                p_business_id    => p_busn_doc_tree_tab_row.business_id
                                );

                END IF;
        END IF;

EXCEPTION
        WHEN others
        THEN
                IF cur_busn_trx%ISOPEN
                THEN
                    CLOSE cur_busn_trx;
                END IF;

        RAISE;
END add_build_stk_busn_trx_seq_sp;

------------------------------------------------------------------------------------------------

PROCEDURE add_pure_build_stk_busn_trx_sp(
        p_busn_doc_tree_tab_row IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row,
        p_target_business_id    IN ods_business_transactions.business_id%TYPE,
        p_process_name          IN ods_business_doc_tree_ref_wrk.created_by%TYPE,
        p_process_date          IN ods_business_doc_tree_ref_wrk.created_date%TYPE
        )
IS
        v_ref_busn_trx_business_id      ods_business_transactions.business_id%TYPE;
        v_ref_busn_trx_business_seq     ods_business_transactions.business_sequence%TYPE;
        v_busn_sequence                 ods_business_transactions.business_sequence%TYPE;
        v_origin_business_id            ods_business_transactions.business_id%TYPE;

        cur_busn_trx                    ods_business_transaction_pkg.t_refcursor;
        v_busn_trx_rec                  t_busn_trx_tab_row;
        v_last_busn_trx_rec             t_busn_trx_tab_row;

        v_parent_doc_comm_loc           ods_business_transactions.parent_doc_comm_loc%TYPE;
        v_parent_doc_year               ods_business_transactions.parent_doc_year%TYPE;
        v_parent_doc_portfolio_type     ods_business_transactions.parent_doc_portfolio_type%TYPE;
        v_parent_doc_month              ods_business_transactions.parent_doc_month%TYPE;
        v_parent_doc_type_code          ods_business_transactions.parent_doc_type_code%TYPE;
        v_parent_doc_number             ods_business_transactions.parent_doc_number%TYPE;
        v_parent_doc_item_num           ods_business_transactions.parent_doc_item_num%TYPE;
        v_parent_doc_split_num          ods_business_transactions.parent_doc_split_num%TYPE;

        v_reprocess_qty                 ods_business_doc_tree_wrk_err.reprocess_qty%TYPE;
BEGIN
        -- recuperamos los datos del ultimo nodo del business transaction
        get_last_busn_trx_node_sp(
                p_business_id           => p_target_business_id,
                p_busn_trx_tab_row      => v_last_busn_trx_rec
            );

        IF v_last_busn_trx_rec.business_id IS NOT NULL
        THEN
                -- asignamos los valores del registro parent a partir de los datos del ultimo nodo de la cadena al que se le
                -- agrega la ramificacion
                v_parent_doc_comm_loc           := p_busn_doc_tree_tab_row.parent_doc_comm_loc_code;
                v_parent_doc_year               := p_busn_doc_tree_tab_row.parent_doc_year;
                v_parent_doc_portfolio_type     := p_busn_doc_tree_tab_row.parent_doc_portfolio_type;
                v_parent_doc_month              := LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0');
                v_parent_doc_type_code          := p_busn_doc_tree_tab_row.parent_doc_type_code;
                v_parent_doc_number             := LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_num),c_doc_num_length,'0');
                v_parent_doc_item_num           := p_busn_doc_tree_tab_row.parent_doc_item_num;
                v_parent_doc_split_num          := p_busn_doc_tree_tab_row.parent_doc_split_num;


                -- damos de alta el business id del business transaction a modificar en la tabla ods_business_doc_tree_wrk para que se
                --  tenga en cuenta como cadena modificada en los procesos subsiguientes
                ods_business_doc_tree_pkg.insert_busn_doc_tree_wrk_sp(
                        p_business_id   => p_target_business_id,
                        p_created_by    => p_process_name
                        );

                -- ajustamos la secuencia del nuevo nodo en base al ultimo nodo encontrado
                v_busn_sequence:=    v_last_busn_trx_rec.business_sequence + 1;

                -- asignamos el business_id original
                v_origin_business_id:= p_busn_doc_tree_tab_row.business_id;

                -- verificamos la existencia de un business transaction correspondiente al nodo del arbol
                -- completamos con ceros a la izquierda el doc_num del nodo del arbol para igualarlo al de busness trx
                get_busn_id_sp(
                        p_doc_comm_location_code        => p_busn_doc_tree_tab_row.doc_comm_loc_code,
                        p_document_year                 => p_busn_doc_tree_tab_row.doc_year,
                        p_document_month                => LPAD(TRIM(p_busn_doc_tree_tab_row.doc_month),c_doc_month_length,'0'),
                        p_doc_type_code                 => p_busn_doc_tree_tab_row.doc_type_code,
                        p_document_number               => LPAD(TRIM(p_busn_doc_tree_tab_row.doc_num),c_doc_num_length,'0'),
                        p_document_item_num             => p_busn_doc_tree_tab_row.doc_item_num,
                        p_document_split_num            => p_busn_doc_tree_tab_row.doc_split_num,
                        p_business_id                   => v_ref_busn_trx_business_id,
                        p_business_sequence             => v_ref_busn_trx_business_seq
                        );

                -- si se encuentra un registro , recuperamos la secuencia completa de transacciones
                IF v_ref_busn_trx_business_id IS NOT NULL
                THEN
                        -- recuperamos la cadena
                        get_row_busn_trx_sp(
                                p_business_id           => v_ref_busn_trx_business_id,
                                p_business_sequence     => v_ref_busn_trx_business_seq,
                                p_cur_busn_trx          => cur_busn_trx
                                );

                        -- recorremos el cursor dando de alta registros en business transaction
                        FETCH cur_busn_trx INTO v_busn_trx_rec;
                        LOOP
                                EXIT WHEN cur_busn_trx%NOTFOUND;
                                -- modificamos los datos que cambian
                                -- NOTA: los campos doc_portfolio_type, doc_comm_location_code,document_year,document_month,doc_type_code,
                                -- document_number,document_item_num,document_split_num y data_source_system se pasan al cual se recuperan del cursor
                                v_busn_trx_rec.business_reference_id            := v_busn_trx_rec.business_id;
                                v_busn_trx_rec.business_reference_seq           := v_busn_trx_rec.business_sequence;
                                v_busn_trx_rec.business_id                      := p_target_business_id;
                                v_busn_trx_rec.business_sequence                := v_busn_sequence;
                                v_busn_trx_rec.parent_doc_comm_loc              := v_parent_doc_comm_loc;
                                v_busn_trx_rec.parent_doc_year                  := v_parent_doc_year;
                                v_busn_trx_rec.parent_doc_portfolio_type        := v_parent_doc_portfolio_type;
                                v_busn_trx_rec.parent_doc_month                 := v_parent_doc_month;
                                v_busn_trx_rec.parent_doc_type_code             := v_parent_doc_type_code;
                                v_busn_trx_rec.parent_doc_number                := v_parent_doc_number;
                                v_busn_trx_rec.parent_doc_item_num              := v_parent_doc_item_num;
                                v_busn_trx_rec.parent_doc_split_num             := v_parent_doc_split_num;
                                v_busn_trx_rec.historical_link_flag             := v_busn_trx_rec.historical_link_flag;
                                v_busn_trx_rec.secondary_costing_flag           := v_busn_trx_rec.secondary_costing_flag;
                                v_busn_trx_rec.origin_business_id               := v_origin_business_id;
                                v_busn_trx_rec.created_date                     := p_process_date;
                                v_busn_trx_rec.created_by                       := p_process_name;
                                v_busn_trx_rec.last_updated_date                := p_process_date;
                                v_busn_trx_rec.last_updated_by                  := p_process_name;
                                v_busn_trx_rec.deleted_date                     := NULL;
                                v_busn_trx_rec.deleted_by                       := NULL;

                                -- damos de alta el nuevo registro
                                insert_busn_trx_row_sp(
                                        p_insert_busn_trx_tab_row    => v_busn_trx_rec
                                        );

                                v_busn_sequence:= v_busn_sequence + 1;

                                -- almacenamos los datos del proximo registro parent
                                v_parent_doc_comm_loc           := v_busn_trx_rec.doc_comm_location_code;
                                v_parent_doc_year               := v_busn_trx_rec.document_year;
                                v_parent_doc_portfolio_type     := v_busn_trx_rec.doc_portfolio_type;
                                v_parent_doc_month              := v_busn_trx_rec.document_month;
                                v_parent_doc_type_code          := v_busn_trx_rec.doc_type_code;
                                v_parent_doc_number             := v_busn_trx_rec.document_number;
                                v_parent_doc_item_num           := v_busn_trx_rec.document_item_num;
                                v_parent_doc_split_num          := v_busn_trx_rec.document_split_num;

                                -- recuperamos el proximo registro de business transaction
                                FETCH  cur_busn_trx INTO v_busn_trx_rec;
                        END LOOP;

                        CLOSE cur_busn_trx;
                        -- dado que encontamos la cadena referenciada ejecutamos el borrado de la business_id de la tabla de business_id
                        -- sin referencia por si el mismo habia sido dado de alta en algun proceso anterior.
                        ods_busn_doc_tree_error_pkg.delete_error_busn_id_sp(
                                p_business_id    => v_origin_business_id
                                );

                ELSE
                        -- si no encuentra la cadena referenciada se da de alta el nodo de la cadena original no encontrada
                        -- como parte de otra cadena y luego se almacenan los datos para loguearlo posteriormente
                        -- como un error.

                        -- tambien se loguea el business id y se verifica si se debe eliminar de la tabla ods_business_doc_tree_wrk_err

                        -- generamos el ultimo nodo de la cadena original (nodo de tipo link S)
                        v_busn_trx_rec.business_id              := p_target_business_id;
                        v_busn_trx_rec.business_sequence        := v_busn_sequence;
                        v_busn_trx_rec.business_reference_id    := c_invalid_busn_trx_ref_id;
                        v_busn_trx_rec.business_reference_seq   := c_invalid_busn_trx_ref_seq;
                        v_busn_trx_rec.doc_comm_location_code   := p_busn_doc_tree_tab_row.doc_comm_loc_code;
                        v_busn_trx_rec.document_year            := p_busn_doc_tree_tab_row.doc_year;
                        v_busn_trx_rec.doc_portfolio_type       := p_busn_doc_tree_tab_row.doc_portfolio_type;
                        v_busn_trx_rec.document_month           := LPAD(TRIM(p_busn_doc_tree_tab_row.doc_month),c_doc_month_length,'0');
                        v_busn_trx_rec.doc_type_code            := p_busn_doc_tree_tab_row.doc_type_code;
                        v_busn_trx_rec.document_number          := LPAD(TRIM(p_busn_doc_tree_tab_row.doc_num),c_doc_num_length,'0');
                        v_busn_trx_rec.document_item_num        := p_busn_doc_tree_tab_row.doc_item_num;
                        v_busn_trx_rec.document_split_num       := p_busn_doc_tree_tab_row.doc_split_num;
                        v_busn_trx_rec.parent_doc_comm_loc      := v_parent_doc_comm_loc;
                        v_busn_trx_rec.parent_doc_year          := v_parent_doc_year;
                        v_busn_trx_rec.parent_doc_portfolio_type:= v_parent_doc_portfolio_type;
                        v_busn_trx_rec.parent_doc_month         := v_parent_doc_month;
                        v_busn_trx_rec.parent_doc_type_code     := v_parent_doc_type_code;
                        v_busn_trx_rec.parent_doc_number        := v_parent_doc_number;
                        v_busn_trx_rec.parent_doc_item_num      := v_parent_doc_item_num;
                        v_busn_trx_rec.parent_doc_split_num     := v_parent_doc_split_num;
                        v_busn_trx_rec.data_source_system       := p_busn_doc_tree_tab_row.data_source_system;
                        v_busn_trx_rec.historical_link_flag     := p_busn_doc_tree_tab_row.historical_link_flag;
                        v_busn_trx_rec.secondary_costing_flag   := p_busn_doc_tree_tab_row.secondary_costing_flag;
                        v_busn_trx_rec.origin_business_id       := v_origin_business_id;
                        --
                        v_busn_trx_rec.document_family_code     := p_busn_doc_tree_tab_row.doc_family_code;
                        --v_busn_trx_rec.document_family_code     := NVL(p_busn_doc_tree_tab_row.doc_family_code,p_busn_doc_tree_tab_row.parent_family_code);
                        --
                        v_busn_trx_rec.unlinked_doc_flag        := 'N';
                        v_busn_trx_rec.integration_type         := p_busn_doc_tree_tab_row.integration_type;
                        v_busn_trx_rec.created_date             := p_process_date;
                        v_busn_trx_rec.created_by               := p_process_name;
                        v_busn_trx_rec.last_updated_date        := p_process_date;
                        v_busn_trx_rec.last_updated_by          := p_process_name;
                        v_busn_trx_rec.deleted_date             := NULL;
                        v_busn_trx_rec.deleted_by               := NULL;

                        -- damos de alta el nodo
                        insert_busn_trx_row_sp(
                                p_insert_busn_trx_tab_row    => v_busn_trx_rec
                                );

                        -- registramos los datos del nodo padre del nodo que no se encuentra como referencia
                        ods_busn_doc_tree_error_pkg.add_error_sp(
                                p_business_id                   => v_origin_business_id,
                                p_parent_doc_comm_loc_code      => p_busn_doc_tree_tab_row.parent_doc_comm_loc_code,
                                p_parent_doc_portfolio_type     => p_busn_doc_tree_tab_row.parent_doc_portfolio_type,
                                p_parent_doc_year               => p_busn_doc_tree_tab_row.parent_doc_year,
                                p_parent_doc_month              => LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0'),
                                p_parent_doc_type_code          => p_busn_doc_tree_tab_row.parent_doc_type_code,
                                p_parent_doc_num                => LPAD(TRIM(p_busn_doc_tree_tab_row.parent_doc_num),c_doc_num_length,'0'),
                                p_parent_doc_item_num           => p_busn_doc_tree_tab_row.parent_doc_item_num,
                                p_parent_doc_split_num          => p_busn_doc_tree_tab_row.parent_doc_split_num,
                                p_error_desc                    => c_busn_trx_ref_no_found_desc,
                                p_error_generation_date         => SYSDATE,
                                p_process_name                  => c_replace_busn_trx_process
                                );

                        -- registramos el business id para reproceso
                        ods_busn_doc_tree_error_pkg.record_error_busn_id_sp(
                                p_business_id   => v_origin_business_id,
                                p_username      => c_replace_busn_trx_process,
                                p_reprocess_qty => v_reprocess_qty
                                );

                        -- verificamos si debemos eliminar el business id de la tabla ods_business_doc_tree_wrk_err
                        IF v_reprocess_qty = ods_busn_doc_tree_error_pkg.c_max_reprocess_qty
                        THEN
                                -- eliminamos el registro
                                ods_busn_doc_tree_error_pkg.delete_error_busn_id_sp(
                                        p_business_id    => v_origin_business_id
                                        );

                        END IF;
                END IF;
        END IF;

EXCEPTION
        WHEN others
        THEN
                IF cur_busn_trx%ISOPEN
                THEN
                    CLOSE cur_busn_trx;
                END IF;

        RAISE;
END add_pure_build_stk_busn_trx_sp;

------------------------------------------------------------------------------------------------

PROCEDURE process_build_to_order_trx_sp(
        p_business_id    ods_business_doc_tree_wrk.business_id%TYPE
        )
IS
        cur_busn_doc_tree               ods_business_doc_tree_pkg.t_refcursor;
        v_busn_doc_tree_tab_row         ods_business_doc_tree_pkg.t_doc_tree_node_tab_row;
        v_parent_busn_doc_tree_tab_row  ods_business_doc_tree_pkg.t_doc_tree_node_tab_row;
        v_child_busn_doc_tree_tab_row   ods_business_doc_tree_pkg.t_doc_tree_node_tab_row;
        v_conf_busn_doc_tree_tab_row    ods_business_doc_tree_pkg.t_doc_tree_node_tab_row;
        v_root_busn_doc_tree_id         ods_business_document_tree.business_document_tree_id%TYPE;

        v_busn_sequence                 ods_business_transactions.business_sequence%TYPE:= 1;
        v_is_root_node                  BOOLEAN:= TRUE;
        v_process_date                  DATE:= SYSDATE;

        v_build_to_stock_flag           BOOLEAN:= FALSE;
        v_pure_stock_link_flag          BOOLEAN:= FALSE;
        v_invalid_node_flag             BOOLEAN:= FALSE;
        v_last_build_to_stk_node_lev    PLS_INTEGER;
        v_last_hist_link_node_level     PLS_INTEGER;
        v_last_node_level               PLS_INTEGER;

        v_parent_doc_comm_loc_code      ods_business_document_tree.parent_doc_comm_loc_code%TYPE;
        v_parent_doc_year               ods_business_document_tree.parent_doc_year%TYPE;
        v_parent_doc_portfolio_type     ods_business_document_tree.parent_doc_portfolio_type%TYPE;
        v_parent_doc_month              ods_business_document_tree.parent_doc_month%TYPE;
        v_parent_doc_type_code          ods_business_document_tree.parent_doc_type_code%TYPE;
        v_parent_doc_num                ods_business_document_tree.parent_doc_num%TYPE;
        v_parent_doc_item_num           ods_business_document_tree.parent_doc_item_num%TYPE;
        v_parent_doc_split_num          ods_business_document_tree.parent_doc_split_num%TYPE;

        v_busn_trx_business_id          ods_business_transactions.business_id%TYPE;
        v_busn_trx_business_seq         ods_business_transactions.business_sequence%TYPE;

        v_mill_ack_portfolio_type       ods_business_transactions.doc_portfolio_type%TYPE;
        v_automatic_mill_ack_gen_flag   ods_common_pkg.s_str_yes_no;
        --
        v_errm	                        VARCHAR2(250);
        v_process	                VARCHAR2(250);
        --
        v_process_bts_flag              ods_common_pkg.s_str_yes_no := ods_common_pkg.c_str_no;
        v_busn_doc_tree_tab_row_bts     ods_business_doc_tree_pkg.t_doc_tree_node_tab_row;
        v_busn_doc_tree_bts_table       ods_buss_doc_tree_table_type := ods_buss_doc_tree_table_type();

BEGIN
        -- generamos el savepoint
        SAVEPOINT sv_bef_generation;

        -- borramos los registros de business transactions que
        -- tengan el mismo business id dado que se van a regenerar
        remove_busn_trx_by_bid_sp(
                p_business_id => p_business_id
                );


        --seteamos en la tabla ODS_BUSN_TRANSACTION_EXTORNOS el código 999999
        --para los casos en donde a la cadena del Bid se le agregaron los documentos
        --por extorno, para que vuelva a procesar el Business.
        update ODS_BUSN_TRANSACTION_EXTORNOS ex
        set     ex.BUSINESS_ID = 999999,
                ex.LAST_UPDATED_DATE = sysdate,
                ex.LAST_UPDATED_BY = 'BldToOrdSp'
        where   ex.BUSINESS_ID = p_business_id;

--        if sql%rowcount > 0 then
--        	dbms_output.PUT_LINE('Update ODS_BUSN_TRANSACTION_EXTORNOS - business_id '||to_char(p_business_id));
--		end if;

        -- obtenemos el arbol de documentos asociado al business id
        -- obtenemos el identificador del nodo raiz del arbol de doc
        v_root_busn_doc_tree_id:= ods_business_doc_tree_pkg.find_doc_tree_root_doc_id_fn(
                p_business_id => p_business_id
                );
        -- recuperamos el arbol de documentos
        ods_business_doc_tree_pkg.get_row_busn_doc_tree_sp(
                p_business_document_tree_id     => v_root_busn_doc_tree_id,
                p_cur_busn_doc_tree             => cur_busn_doc_tree
                );

        -- recorremos el arbol generando los nuevos registros de business transaction
        IF cur_busn_doc_tree%ISOPEN
        THEN
                FETCH cur_busn_doc_tree INTO v_busn_doc_tree_tab_row;

                -- se verifica si se trata de un nodo parent / millack invalido
                IF invalid_parent_millack_node_fn(
                        p_busn_doc_tree_tab_row    => v_busn_doc_tree_tab_row
                        )
                THEN
                        -- seteamos el flag
                        v_invalid_node_flag:= TRUE;

                        -- borramos el business document tree correspondiente
                        ods_business_doc_tree_pkg.delete_busn_doc_tree_sp(
                                p_business_id => v_busn_doc_tree_tab_row.business_id
                                );
                END IF;

		-- antes del procesamiento normal se verifica si se trata de un nodo Puro Stock (S)
                IF    NOT v_invalid_node_flag
                  AND ods_business_doc_tree_pkg.is_build_to_stock_doc_tree_fn(
                        p_business_document_tree_id => v_busn_doc_tree_tab_row.business_document_tree_id
                        )
                THEN
                        -- si se trata de una cadena Puro Stock (S) verificamos si el nodo parent se encuentra presente en alguna
                        -- cadena  BUSINESS TRANSACTION
                        get_busn_id_sp(
                                p_doc_comm_location_code        => v_busn_doc_tree_tab_row.parent_doc_comm_loc_code,
                                p_document_year                 => v_busn_doc_tree_tab_row.parent_doc_year,
                                p_document_month                => LPAD(TRIM(v_busn_doc_tree_tab_row.parent_doc_month),c_doc_month_length,'0'),
                                p_doc_type_code                 => v_busn_doc_tree_tab_row.parent_doc_type_code,
                                p_document_number               => LPAD(TRIM(v_busn_doc_tree_tab_row.parent_doc_num),c_doc_num_length,'0'),
                                p_document_item_num             => v_busn_doc_tree_tab_row.parent_doc_item_num,
                                p_document_split_num            => v_busn_doc_tree_tab_row.parent_doc_split_num,
                                p_business_id                   => v_busn_trx_business_id,
                                p_business_sequence             => v_busn_trx_business_seq
                                );

                        IF v_busn_trx_business_id IS NOT NULL
                        THEN
                                -- si encontramos una cadena con el documento parent cambiamos el flag para indicar que no se debe
                                -- seguir con el proceso normal de recorrido e insercion de nodos
                                v_pure_stock_link_flag:= TRUE;

                                -- buscamos el registro de tipo Stock
                                WHILE    NVL(v_busn_doc_tree_tab_row.link_type,ods_common_pkg.c_str_null) <> 'S'
                                     AND cur_busn_doc_tree%FOUND
                                LOOP
                                        -- recuperamos el siguiente registro
                                        FETCH cur_busn_doc_tree INTO v_busn_doc_tree_tab_row;
                                END LOOP;


                                -- debemos verificar si dentro de esa cadena no existe el nodo hijo.
                                -- si se encuentra no se hace nada con esta cadena dado que ya fue agregado como una ramificacion de otra cadena
                                IF NOT exist_ref_busn_trx_fn(
                                                p_business_id                   => v_busn_trx_business_id,
                                                p_doc_comm_location_code        => v_busn_doc_tree_tab_row.doc_comm_loc_code,
                                                p_document_year                 => v_busn_doc_tree_tab_row.doc_year,
                                                p_document_month                => LPAD(TRIM(v_busn_doc_tree_tab_row.doc_month),c_doc_month_length,'0'),
                                                p_doc_type_code                 => v_busn_doc_tree_tab_row.doc_type_code,
                                                p_document_number               => LPAD(TRIM(v_busn_doc_tree_tab_row.doc_num),c_doc_num_length,'0'),
                                                p_document_item_num             => v_busn_doc_tree_tab_row.doc_item_num,
                                                p_document_split_num            => v_busn_doc_tree_tab_row.doc_split_num
                                                )
                                THEN

                                        -- si no se encuentra se debe agregar esta cadena como ramificacion de la cadena en las que se encontrol el nodo parent
                                        -- invocamos el proceso
                                        v_process := 'add_pure_build_stk_busn_trx_sp';
                                        add_pure_build_stk_busn_trx_sp(
                                                p_busn_doc_tree_tab_row => v_busn_doc_tree_tab_row,
                                                p_target_business_id    => v_busn_trx_business_id,
                                                p_process_name          => c_stk_branch_busn_trx_process,
                                                p_process_date          => v_process_date
                                                );
                                ELSE
                                        -- si se encuentra significa que ya fue agregado como una ramificacion y no se debe hacer nada
                                        NULL;

                                END IF;

                        END IF;
                END IF;

                -- Si en caso contrario se procede a generar de la forma normal para las cadenas que no son Puro Stock
                IF NOT v_pure_stock_link_flag AND NOT v_invalid_node_flag
                THEN
                        LOOP
                                EXIT WHEN cur_busn_doc_tree%NOTFOUND;

                                IF   NOT v_build_to_stock_flag
                                  OR (v_build_to_stock_flag AND  v_busn_doc_tree_tab_row.doc_node_level <= v_last_build_to_stk_node_lev)
                                THEN
                    			-- verificamos si el portfolio del nodo parent es 98 o 99
                                        -- si es asi debemos sobreescribir los datos del nodo parent para que no se corte el enlace
                                        -- para el caso en que el primer nodo parent tiene portfolio 98 o 99 se asignara las variable en NULL
                                        IF v_busn_doc_tree_tab_row.parent_doc_portfolio_type IN ('98','99')
                                        THEN
                                                v_busn_doc_tree_tab_row.parent_doc_comm_loc_code        := v_parent_doc_comm_loc_code;
                                                v_busn_doc_tree_tab_row.parent_doc_year                 := v_parent_doc_year;
                                                v_busn_doc_tree_tab_row.parent_doc_portfolio_type       := v_parent_doc_portfolio_type;
                                                v_busn_doc_tree_tab_row.parent_doc_month                := v_parent_doc_month;
                                                v_busn_doc_tree_tab_row.parent_doc_type_code            := v_parent_doc_type_code;
                                                v_busn_doc_tree_tab_row.parent_doc_num                  := v_parent_doc_num;
                                                v_busn_doc_tree_tab_row.parent_doc_item_num             := v_parent_doc_item_num;
                                                v_busn_doc_tree_tab_row.parent_doc_split_num            := v_parent_doc_split_num;
                                        END IF;

                                        -- verificamos si es un historical link, si es asi cambiamos el flag a Yes para que se
                                        -- propague en toda la rama hasta la hoja, en caso contrario se asigna el valor original (nulo o NO)
                                        -- si esta marcado como historical link y ademas es el de menor nivel almacenamos el nivel donde se encuentra marcado
                                        IF     NVL(v_busn_doc_tree_tab_row.historical_link_flag,ods_common_pkg.c_str_null) = ods_common_pkg.c_str_yes
                                           AND v_busn_doc_tree_tab_row.doc_node_level < NVL(v_last_hist_link_node_level,9999999)
                                        THEN
                                                v_last_hist_link_node_level:= v_busn_doc_tree_tab_row.doc_node_level;
                                        ELSE
                                                -- cada vez que cambiamos de rama verificamos si se debe seguir manteniendo el valor
                                                -- del historical link
                                                IF NVL(v_last_node_level,0) >= v_busn_doc_tree_tab_row.doc_node_level
                                                THEN
                                                        -- si el nivel donde se empezo a aplicar el historical link es mayor se debe resetear,
                                                        -- el nivel de comparacion sino se deje igual para que se siga propagando
                                                        IF v_last_hist_link_node_level >= v_busn_doc_tree_tab_row.doc_node_level
                                                        THEN
                                                                v_last_hist_link_node_level:= 9999999;
                                                        END IF;
                                                END IF;
                                        END IF;

                    			-- verificamos primero si se trata de un nodo Build to Stock
                                        IF v_busn_doc_tree_tab_row.link_type = 'S'
                                        THEN
                                                IF v_last_hist_link_node_level <= v_busn_doc_tree_tab_row.doc_node_level
                                                THEN
                                                        v_busn_doc_tree_tab_row.historical_link_flag:= ods_common_pkg.c_str_yes;
                                                END IF;

                                                --Resguardamos el nodo BTS para insertalo despues de procesada la cadena
                                                v_busn_doc_tree_bts_table.EXTEND;
                                                v_busn_doc_tree_bts_table (v_busn_doc_tree_bts_table.COUNT) :=
                                                        ods_buss_doc_tree_row_type (
                                                                v_busn_doc_tree_tab_row.business_document_tree_id,
                                                                v_busn_doc_tree_tab_row.doc_node_level,
                                                                v_busn_doc_tree_tab_row.business_id,
                                                                v_busn_doc_tree_tab_row.parent_doc_comm_loc_code,
                                                                v_busn_doc_tree_tab_row.parent_doc_portfolio_type,
                                                                v_busn_doc_tree_tab_row.parent_doc_year,
                                                                v_busn_doc_tree_tab_row.parent_doc_month,
                                                                v_busn_doc_tree_tab_row.parent_doc_type_code,
                                                                v_busn_doc_tree_tab_row.parent_doc_num,
                                                                v_busn_doc_tree_tab_row.parent_doc_item_num,
                                                                v_busn_doc_tree_tab_row.parent_doc_split_num,
                                                                v_busn_doc_tree_tab_row.doc_comm_loc_code,
                                                                v_busn_doc_tree_tab_row.doc_portfolio_type,
                                                                v_busn_doc_tree_tab_row.doc_year,
                                                                v_busn_doc_tree_tab_row.doc_month,
                                                                v_busn_doc_tree_tab_row.doc_type_code,
                                                                v_busn_doc_tree_tab_row.doc_num,
                                                                v_busn_doc_tree_tab_row.doc_item_num,
                                                                v_busn_doc_tree_tab_row.doc_split_num,
                                                                v_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code,
                                                                v_busn_doc_tree_tab_row.mill_ack_doc_year,
                                                                v_busn_doc_tree_tab_row.mill_ack_doc_month,
                                                                v_busn_doc_tree_tab_row.mill_ack_doc_type_code,
                                                                v_busn_doc_tree_tab_row.mill_ack_doc_num,
                                                                v_busn_doc_tree_tab_row.mill_ack_item_num,
                                                                v_busn_doc_tree_tab_row.link_type,
                                                                v_busn_doc_tree_tab_row.data_source_system,
                                                                v_busn_doc_tree_tab_row.historical_link_flag,
                                                                v_busn_doc_tree_tab_row.secondary_costing_flag,
                                                                v_busn_doc_tree_tab_row.parent_family_code,
                                                                v_busn_doc_tree_tab_row.doc_family_code,
                                                                v_busn_doc_tree_tab_row.integration_type,
                                                                v_busn_doc_tree_tab_row.created_by,
                                                                v_busn_doc_tree_tab_row.parent_man_flag,
                                                                v_busn_doc_tree_tab_row.doc_man_flag,
                                                                v_busn_doc_tree_tab_row.mill_ack_man_flag,
                                                                v_busn_doc_tree_tab_row.mill_ack_split_num
                                                                --
                                                                --v_busn_doc_tree_tab_row.mill_ack_family_code
                                                                --
                                                                );
                                                --
                                                ---- procesamos el nodo Build to Stock
                                                --v_process := 'add_build_stk_busn_trx_seq_sp';
                                                --add_build_stk_busn_trx_seq_sp(
                                                --        p_busn_doc_tree_tab_row => v_busn_doc_tree_tab_row,
                                                --        p_busn_sequence         => v_busn_sequence,
                                                --        p_process_name          => c_replace_busn_trx_process,
                                                --        p_process_date          => v_process_date
                                                --        );

                                                --
                                                v_process_bts_flag:= ods_common_pkg.c_str_yes;
                                                --
                                                v_build_to_stock_flag:= TRUE;
                                                v_last_build_to_stk_node_lev:= v_busn_doc_tree_tab_row.doc_node_level;

                                        ELSE

                                                v_build_to_stock_flag:= FALSE;
                                                -- se trata de un nodo No Build to Stock
                                                -- verificamos si debemos o no generar el registro

                                                -- verificamos para el nodo raiz
                                                -- 98 Y 99 son tipos de portfolios especiales para unir SAP con TEN
                                                IF v_is_root_node
                                                AND v_busn_doc_tree_tab_row.parent_doc_portfolio_type NOT IN ('98','99')
                                                THEN
                                                    -- ejecutamos el proceso de ajuste de datos segun datos manuales
                                                    ods_business_doc_tree_pkg.adjust_manual_doc_tree_data_sp (
                                                                p_orig_busn_doc_tree_row        => v_busn_doc_tree_tab_row,
                                                                p_node_type                     => ods_business_doc_tree_pkg.c_parent_doc_tree_node,
                                                                p_adj_busn_doc_tree_row         => v_parent_busn_doc_tree_tab_row
                                                                );

                                                        -- insertamos el registro del nodo padre
                                                        -- se le asigna como usuario de creacion el usuario de creacion del nodo business doc tree
                                                        v_process := 'add_busn_trx_root_record_sp';
                                                        add_busn_trx_root_record_sp(
                                                                p_busn_doc_tree_tab_row => v_parent_busn_doc_tree_tab_row,
                                                                p_busn_sequence         => v_busn_sequence,
                                                                p_process_name          => v_parent_busn_doc_tree_tab_row.created_by,
                                                                p_process_date          => v_process_date
                                                                );
                                                        v_busn_sequence:= v_busn_sequence + 1;

                                                END IF;

                                                IF v_last_hist_link_node_level <= v_busn_doc_tree_tab_row.doc_node_level
                                                THEN
                                                        v_busn_doc_tree_tab_row.historical_link_flag:= ods_common_pkg.c_str_yes;
                                                END IF;
                                                -- verificamos para un nodo conferma, si existe lo damos de alta siempre
                                                -- antes del nodo hijo
                                                IF v_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code IS NOT NULL
                                                AND TRIM(v_busn_doc_tree_tab_row.mill_ack_doc_num) IS NOT NULL
                                                AND v_busn_doc_tree_tab_row.mill_ack_doc_num <> c_null_mill_ack_doc_num
                                                THEN
                                                        --
                                                        -- ejecutamos el proceso de ajuste de datos segun datos manuales
                                                        ods_business_doc_tree_pkg.adjust_manual_doc_tree_data_sp (
                                                                p_orig_busn_doc_tree_row        => v_busn_doc_tree_tab_row,
                                                                p_node_type                     => ods_business_doc_tree_pkg.c_confirm_doc_tree_node ,
                                                                p_adj_busn_doc_tree_row         => v_conf_busn_doc_tree_tab_row
                                                                );
                                                        --
                                                        -- insertamos el registro del nodo conferma
                                                        -- verificamos el portfolio type a asignar segun la sociedad
                                                        v_mill_ack_portfolio_type := ods_bt_societies_pkg.get_mill_ack_portf_fn(
                                                                                        p_commercial_location_code => v_conf_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code,
                                                                                        p_data_source_system => v_conf_busn_doc_tree_tab_row.data_source_system
                                                                                        );
                                                        --
                                                        IF NVL(v_mill_ack_portfolio_type,ods_common_pkg.c_invalid_portfolio_type) = c_prod_operat_portfolio_type
                                                        THEN
                                                                -- si la sociedad tiene portfolio 06 entonces asignamos el portfolio 88 mill ack
                                                                -- sino dejamos el que posee el arbol doc tree
                                				v_mill_ack_portfolio_type:= c_mill_ack_portfolio_type;

                                                                -- en caso contrario se pasa este mismo valor obtenido, aunque sea NULO
                                                        END IF;

                                                        --
                                                        v_process := 'add_busn_trx_ack_record_sp: 1-v_mill_ack_portfolio_type: ' || v_mill_ack_portfolio_type ||
                                                                     ' v_conf_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code: '|| v_conf_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code ||
                                                                     ' v_conf_busn_doc_tree_tab_row.data_source_system: '|| v_conf_busn_doc_tree_tab_row.data_source_system;
                                                        --
                                                        --Generamos el enganche con la conferma mientras el Portfolio de la Mill ACK (que se obtiene de la
                                                        --ODS_BT_SOCIETIES mediante la funcion ods_bt_societies_pkg.get_mill_ack_portf_fn) sea distinto de NULL
                                                        --caso contrario considera al nodo como invalido y continua con el siguiente nodo de la Document Tree.
                                                        IF v_mill_ack_portfolio_type IS NOT NULL
                                                        THEN
                                                                -- se le asigna como usuario de creacion el usuario de creacion del nodo business doc tree
                                                                add_busn_trx_ack_record_sp(
                                                                        p_busn_doc_tree_tab_row         => v_conf_busn_doc_tree_tab_row,
                                                                        p_busn_sequence                 => v_busn_sequence,
                                                                        p_mill_ack_portfolio_type       => v_mill_ack_portfolio_type,
                                                                        p_process_name                  => v_conf_busn_doc_tree_tab_row.created_by,
                                                                        p_process_date                  => v_process_date
                                                                        );
                                                                --
                                                                v_busn_sequence:= v_busn_sequence + 1;
                                                                --
                                                                -- verificamos si tenemos que dar de alta el nodo conferma adicional
                                                                v_automatic_mill_ack_gen_flag := ods_bt_societies_pkg.get_auto_mill_ack_gen_flag_fn(
                                                                        p_commercial_location_code      => v_conf_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code,
                                                                        p_data_source_system            => v_conf_busn_doc_tree_tab_row.data_source_system,
                                                                        p_last_search_flag              => ods_common_pkg.c_str_true
                                                                        );
                                                                --
							        IF NVL(v_automatic_mill_ack_gen_flag , ods_common_pkg.c_str_no) = ods_common_pkg.c_str_yes
                                                                THEN
                                                                        -- damos de alta el nodo conferma adicional
                                                                        v_process := 'add_busn_trx_add_ack_record_sp: 2-v_mill_ack_portfolio_type: ' || v_mill_ack_portfolio_type ||
                                                                                     ' v_conf_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code: '|| v_conf_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code ||
                                                                                     ' v_conf_busn_doc_tree_tab_row.data_source_system: '|| v_conf_busn_doc_tree_tab_row.data_source_system ||
                                                                                     ' v_automatic_mill_ack_gen_flag: '|| v_automatic_mill_ack_gen_flag;
                                                                        --
                                                                        add_busn_trx_add_ack_record_sp(
                                                                                p_busn_doc_tree_tab_row         => v_conf_busn_doc_tree_tab_row,
                                                                                p_busn_sequence                 => v_busn_sequence,
                                                                                p_mill_ack_portfolio_type       => v_mill_ack_portfolio_type,
                                                                                p_process_name                  => v_conf_busn_doc_tree_tab_row.created_by,
                                                                                p_process_date                  => v_process_date
                                                                                );
                                                                        --
                                                                        v_busn_sequence:= v_busn_sequence + 1;
                                                                        --
                                                                END IF;
                                                                --
                                                        ELSE
                                                                --
                                                                --Sino se pudo generar el enganche con la Conferma logueamos el ID de la Document Tree y su BID
                                                                ods_error_log_pkg.insert_error_log_table (
                                                                        p_process               => 'PROCESS_BUILD_TO_ORDER_TRX_SP: Generar enganche con la Conferma',
                                                                        p_description           => 'No pudo generar el enganche con la Conferma. BID: ' || v_conf_busn_doc_tree_tab_row.business_id || 'Busn Doc Tree Id: ' || v_conf_busn_doc_tree_tab_row.business_document_tree_id,
                                                                        p_description2          => v_conf_busn_doc_tree_tab_row.business_document_tree_id,
                                                                        p_created_by            => USER,
                                                                        p_last_updated_date     => SYSDATE,
                                                                        p_last_updated_by       => USER);
                                                                --
                                                        END IF;
                                                        --
                                                END IF;

                                                -- verificamos para un nodo hijo no raiz
                                                -- 98 Y 99 son tipos de portfolios especiales para unir SAP con TEN
                                                IF v_busn_doc_tree_tab_row.doc_comm_loc_code IS NOT NULL
                                                AND TRIM(v_busn_doc_tree_tab_row.doc_item_num) IS NOT NULL
                                                AND v_busn_doc_tree_tab_row.doc_portfolio_type NOT IN ('98','99')
                                                THEN

                                                        -- ejecutamos el proceso de ajuste de datos segun datos manuales
                                                        ods_business_doc_tree_pkg.adjust_manual_doc_tree_data_sp (
                                                                p_orig_busn_doc_tree_row        => v_busn_doc_tree_tab_row,
                                                                p_node_type                     => ods_business_doc_tree_pkg.c_child_doc_tree_node ,
                                                                p_adj_busn_doc_tree_row         => v_child_busn_doc_tree_tab_row
                                                                );

                                                        -- insertamos el registro del nodo hijo
                                                        -- se le asigna como usuario de creacion el usuario de creacion del nodo business doc tree
                                                        v_process := 'add_busn_trx_child_record_sp';
                                                        add_busn_trx_child_record_sp(
                                                                p_busn_doc_tree_tab_row => v_child_busn_doc_tree_tab_row,
                                                                p_busn_sequence         => v_busn_sequence,
                                                                p_process_name          => v_child_busn_doc_tree_tab_row.created_by,
                                                                p_process_date          => v_process_date
                                                                );
                                                        v_busn_sequence:= v_busn_sequence + 1;
                                                ELSIF v_busn_doc_tree_tab_row.doc_comm_loc_code IS NOT NULL
                                                AND v_busn_doc_tree_tab_row.doc_item_num IS NOT NULL
                                                AND v_busn_doc_tree_tab_row.doc_portfolio_type IN ('98','99')
                                                THEN
                                                        -- almacenamos los datos del nodo padre para que pueda utilizarse
                                                        -- para enlazar el nodo siguiente
                                                        IF v_busn_doc_tree_tab_row.parent_doc_portfolio_type NOT IN ('98','99')
                                                        THEN
                                                                -- asignamos los valores del registro parent a partir de los datos del nodo de document tree
                                                                v_parent_doc_comm_loc_code      := v_busn_doc_tree_tab_row.parent_doc_comm_loc_code;
                                                                v_parent_doc_year               := v_busn_doc_tree_tab_row.parent_doc_year;
                                                                v_parent_doc_portfolio_type     := v_busn_doc_tree_tab_row.parent_doc_portfolio_type;
                                                                v_parent_doc_month              := v_busn_doc_tree_tab_row.parent_doc_month;
                                                                v_parent_doc_type_code          := v_busn_doc_tree_tab_row.parent_doc_type_code;

                                                                -- dejamos el valor puro dado que despues se aignara para que se contemple como si fuera el valor original proveniente de la tabla de business doc tree
                                                                v_parent_doc_num                := v_busn_doc_tree_tab_row.parent_doc_num;
                                                                v_parent_doc_item_num           := v_busn_doc_tree_tab_row.parent_doc_item_num;
                                                                v_parent_doc_split_num          := v_busn_doc_tree_tab_row.parent_doc_split_num;

                                                        END IF;
                                                END IF;

                                                -- cambiamos el flag para que no se tome ningun nodo padre más
                                                -- este flag no se cambia en el If de nodo padre dado que puede ser que el nodo padre sea 98 o 99 y nunca entre al IF
                                                v_is_root_node:= FALSE;

                                        END IF;


                                END IF;

                                -- almacenamos el nivel del ultimo registro
                                v_last_node_level:= v_busn_doc_tree_tab_row.doc_node_level;
                                FETCH cur_busn_doc_tree INTO v_busn_doc_tree_tab_row;
                        END LOOP;
                        --
                        --Insertamos todos los nodos BTS resguardados para que queden ordenados al final de la cadena procesada
                        IF v_process_bts_flag = ods_common_pkg.c_str_yes
                        THEN
                                --v_process := 'add_build_stk_busn_trx_seq_sp';
                                --
                                FOR v_busn_doc_tree_tab_row_bts IN (SELECT  *
                                                                    FROM    TABLE(CAST(v_busn_doc_tree_bts_table AS ods_buss_doc_tree_table_type)))
                                LOOP
                                        v_process := 'add_build_stk_busn_trx_seq_sp: doc_portfolio: '|| v_busn_doc_tree_tab_row_bts.doc_portfolio_type || ' parent_portfolio: '||v_busn_doc_tree_tab_row_bts.parent_doc_portfolio_type;
                                        --
                                        --Solo se insertaran aquellos nodos del tipo S que tengan datos del documento child informado, es decir que los DOC_XXXX
                                        --sean NOT NULL. Caso contrario logueamos en ODS_LOG_ERROR_TABLE el ID del nodo de la Doc Tree que se quiere enganchar.
                                        IF v_busn_doc_tree_tab_row_bts.doc_portfolio_type IS NOT NULL
                                        THEN
                                                --
                                                -- procesamos el nodo Build to Stock
                                                add_build_stk_busn_trx_seq_sp(
                                                        p_busn_doc_tree_tab_row => v_busn_doc_tree_tab_row_bts,
                                                        p_busn_sequence         => v_busn_sequence,
                                                        p_process_name          => c_replace_busn_trx_process,
                                                        p_process_date          => v_process_date
                                                        );
                                                --
                                        ELSE
                                                --
                                                --Sino se pudo procesar el nodo logueamos el ID de la Document Tree y su BID
                                                ods_error_log_pkg.insert_error_log_table (
                                                        p_process               => 'PROCESS_BUILD_TO_ORDER_TRX_SP: Insercion de nodos BTS resguardados en memoria para que queden ordenados al final de la cadena procesada',
                                                        p_description           => 'No pudo procesar el nodo Build to Stock. BID: ' || v_busn_doc_tree_tab_row_bts.business_id || 'Busn Doc Tree Id: ' || v_busn_doc_tree_tab_row_bts.business_document_tree_id,
                                                        p_description2          => v_busn_doc_tree_tab_row_bts.business_document_tree_id,
                                                        p_created_by            => USER,
                                                        p_last_updated_date     => SYSDATE,
                                                        p_last_updated_by       => USER);
                                                --
                                        END IF;
                                        --
                                END LOOP;
                                --
                        END IF;
                        --
                END IF;
        END IF;

        CLOSE cur_busn_doc_tree;
EXCEPTION
        WHEN e_doc_tree_node_twice_insert
        THEN
                v_errm := SUBSTR(SQLERRM, 1 , 250);
                ODS_ERROR_LOG_PKG.INSERT_ERROR_LOG_TABLE (
                        p_process               => 'PROCESS_BUILD_TO_ORDER_TRX_SP'||' - '||v_process,
                        p_description           => 'EXCEPTION: e_doc_tree_node_twice_insert ' || SQLERRM,
                        p_description2          => p_business_id,
                        p_created_by            => 'TENARIS',
                        p_last_updated_date     => SYSDATE,
                        p_last_updated_by       => 'TENARIS');

                -- hacemos un rollback al punto previo al borrado de los business transactions a regenerar
                ROLLBACK TO sv_bef_generation;

        WHEN others
        THEN

                v_errm := SUBSTR(SQLERRM, 1 , 250);
                ODS_ERROR_LOG_PKG.INSERT_ERROR_LOG_TABLE (
                        p_process               => 'PROCESS_BUILD_TO_ORDER_TRX_SP'||' - '||v_process,
                        p_description           => 'EXCEPTION: others ' || SQLERRM,
                        p_description2          => p_business_id,
                        p_created_by            => 'TENARIS',
                        p_last_updated_date     => SYSDATE,
                        p_last_updated_by       => 'TENARIS');

                IF cur_busn_doc_tree%ISOPEN
                THEN
                        CLOSE cur_busn_doc_tree;
                END IF;

                RAISE;

END process_build_to_order_trx_sp;


--------------------------------------------------------------------------------
PROCEDURE get_busn_id_sp(
        p_doc_comm_location_code        IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_document_year                 IN ods_business_transactions.document_year%TYPE,
        p_document_month                IN ods_business_transactions.document_month%TYPE,
        p_doc_type_code                 IN ods_business_transactions.doc_type_code%TYPE,
        p_document_number               IN ods_business_transactions.document_number%TYPE,
        p_document_item_num             IN ods_business_transactions.document_item_num%TYPE,
        p_document_split_num            IN ods_business_transactions.document_split_num%TYPE,
    	p_historical_flag               IN ods_common_pkg.s_str_boolean DEFAULT ods_common_pkg.c_str_true,
        p_business_id                   OUT ods_business_transactions.business_id%TYPE,
        p_business_sequence             OUT ods_business_transactions.business_sequence%TYPE
        )
IS
        v_business_id           ods_business_transactions.business_id%TYPE;
        v_business_sequence     ods_business_transactions.business_sequence%TYPE;
BEGIN
        BEGIN
                SELECT  business_id,
                        business_sequence
                INTO    v_business_id,
                        v_business_sequence
                FROM    ods_business_transactions
                WHERE   doc_comm_location_code = p_doc_comm_location_code
                AND     document_year = p_document_year
                AND     document_month = p_document_month
                AND     doc_type_code = p_doc_type_code
                AND     document_number = p_document_number
                AND     document_item_num = p_document_item_num
                AND     document_split_num = p_document_split_num
                AND     business_reference_id IS NULL
        	AND     (
                            (p_historical_flag = ods_common_pkg.c_str_true)
                        OR  (    p_historical_flag = ods_common_pkg.c_str_false
                             AND historical_link_flag = ods_common_pkg.c_str_no
                            )
                        )
                AND     ROWNUM = 1;
        EXCEPTION
                WHEN no_data_found
                THEN
        		--dbms_output.PUT_LINE('No se encontró el business_id en ODS_BUSINESS_TRANSACTIONS');
		        -- si no lo encuentra no hace nada y retorna la variable sin valor
                NULL;
        END;

        p_business_id := v_business_id;
        p_business_sequence := v_business_sequence;

END get_busn_id_sp;

--------------------------------------------------------------------------------


FUNCTION exist_ref_busn_trx_fn(
        p_business_id                   IN ods_business_transactions.business_id%TYPE,
        p_doc_comm_location_code        IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_document_year                 IN ods_business_transactions.document_year%TYPE,
        p_document_month                IN ods_business_transactions.document_month%TYPE,
        p_doc_type_code                 IN ods_business_transactions.doc_type_code%TYPE,
        p_document_number               IN ods_business_transactions.document_number%TYPE,
        p_document_item_num             IN ods_business_transactions.document_item_num%TYPE,
        p_document_split_num            IN ods_business_transactions.document_split_num%TYPE
        )
RETURN BOOLEAN
IS
        v_result        BOOLEAN:= TRUE;
        v_dummy         PLS_INTEGER;
BEGIN
        BEGIN
                SELECT  1
                INTO    v_dummy
                FROM    ods_business_transactions
                WHERE   doc_comm_location_code = p_doc_comm_location_code
                AND     document_year = p_document_year
                AND     document_month = p_document_month
                AND     doc_type_code = p_doc_type_code
                AND     document_number = p_document_number
                AND     document_item_num = p_document_item_num
                AND     document_split_num = p_document_split_num
                AND     (   (business_sequence > 1)
                         OR (business_reference_id = c_invalid_busn_trx_ref_id AND business_reference_seq = c_invalid_busn_trx_ref_seq)
                        )
                AND     business_id = p_business_id
                AND     ROWNUM = 1;
        EXCEPTION
                WHEN no_data_found
                THEN
                        v_result:= FALSE;
        END;

        RETURN v_result;

END exist_ref_busn_trx_fn;

--------------------------------------------------------------------------------

PROCEDURE get_row_busn_trx_sp(
        p_business_id           IN ods_business_transactions.business_id%TYPE,
        p_business_sequence     IN ods_business_transactions.business_sequence%TYPE,
        p_cur_busn_trx          OUT t_refcursor
        )
IS
BEGIN
        OPEN p_cur_busn_trx FOR
        SELECT  business_id                ,
                business_sequence          ,
                business_reference_id      ,
                business_reference_seq     ,
                doc_portfolio_type         ,
                doc_comm_location_code     ,
                document_year              ,
                document_month             ,
                doc_type_code              ,
                document_number            ,
                document_item_num          ,
                document_split_num         ,
                unlinked_doc_flag          ,
                data_source_system         ,
                parent_doc_comm_loc        ,
                parent_doc_portfolio_type  ,
                parent_doc_year            ,
                parent_doc_month           ,
                parent_doc_type_code       ,
                parent_doc_number          ,
                parent_doc_item_num        ,
                parent_doc_split_num       ,
                historical_link_flag       ,
                secondary_costing_flag       ,
                origin_business_id       ,
                document_family_code       ,
                DECODE(data_source_system,ods_common_pkg.c_man_data_source, ods_common_pkg.c_str_yes,ods_common_pkg.c_str_no) manual_record_flag,
                integration_type       ,
                created_date               ,
                created_by                 ,
                last_updated_date          ,
                last_updated_by            ,
                deleted_date               ,
                deleted_by
        FROM    ods_business_transactions
        WHERE   business_id = p_business_id
        AND     (
                 (    p_business_sequence IS NOT NULL
                  AND business_sequence >= p_business_sequence
                 )
                 OR p_business_sequence IS NULL
                )
        ORDER BY business_sequence;

END get_row_busn_trx_sp;

--------------------------------------------------------------------------------

PROCEDURE get_row_busn_trx_sp(
        p_business_id   IN ods_business_transactions.business_id%TYPE,
        p_busn_trx_tab  OUT t_busn_trx_tab
        )
IS
        cur_busn_trx    t_refcursor;
        v_busn_trx_rec  t_busn_trx_tab_row;
        i               PLS_INTEGER:= 0;
BEGIN
        -- recuperamos la cadena de transacciones de la referencia
        get_row_busn_trx_sp(
                p_business_id           => p_business_id,
                p_business_sequence     => NULL,
                p_cur_busn_trx          => cur_busn_trx
                );

        IF cur_busn_trx%ISOPEN
        THEN
                FETCH cur_busn_trx INTO v_busn_trx_rec;
                LOOP
                        EXIT WHEN cur_busn_trx%NOTFOUND;
                        i:= i + 1;
                        p_busn_trx_tab(i):= v_busn_trx_rec;
                        FETCH cur_busn_trx INTO v_busn_trx_rec;
                END LOOP;

                CLOSE cur_busn_trx;
        END IF;
EXCEPTION
        WHEN others
        THEN
                IF cur_busn_trx%ISOPEN
                THEN
                        CLOSE cur_busn_trx;
                END IF;

                RAISE;
END get_row_busn_trx_sp;

--------------------------------------------------------------------------------
PROCEDURE regenerate_ref_busn_trx_sp(
        p_busn_trx_tab     IN t_busn_trx_tab
        )
IS
        v_orig_busn_trx_tab    t_busn_trx_tab;
        v_new_busn_trx_tab         t_busn_trx_tab;

        i PLS_INTEGER;
        j PLS_INTEGER:= 0;

        v_busn_sequence            ods_business_transactions.business_sequence%TYPE;
        cur_ref_busn_trx            ods_business_transaction_pkg.t_refcursor;
        v_ref_busn_trx_rec            t_busn_trx_tab_row;
        v_found_ref_busn_trx_flag     BOOLEAN:= FALSE;
        v_last_processed_busn_ref_id    ods_business_transactions.business_reference_id%TYPE:= c_invalid_busn_ref_id;
        v_business_id                ods_business_transactions.business_id%TYPE;

BEGIN
    -- generamos el savepoint
        SAVEPOINT sv_bef_ref_busn_trx_regen;

        -- asignamos la cadena original a la variable
        v_orig_busn_trx_tab:= p_busn_trx_tab;

        -- recorremos la tabla copiando y verificando si se debe regenerar
        i:= v_orig_busn_trx_tab.FIRST;

        WHILE i IS NOT NULL
        LOOP
                BEGIN
                        -- inicializamos la secuencia para la cadena nueva y el business id
                        IF v_busn_sequence IS NULL
                        THEN
                                v_busn_sequence:= 0;
                                v_business_id:= v_orig_busn_trx_tab(i).business_id;

                        END IF;

                        IF v_orig_busn_trx_tab(i).business_reference_id IS NOT NULL
                        AND v_last_processed_busn_ref_id <> v_orig_busn_trx_tab(i).business_reference_id
                        AND ods_business_doc_tree_pkg.exist_doc_tree_work_fn(v_orig_busn_trx_tab(i).business_reference_id)
                        THEN
                                v_last_processed_busn_ref_id:= v_orig_busn_trx_tab(i).business_reference_id;

                                -- recuperamos la cadena de transacciones referenciadas y la copiamos en la tabla plsql
                                get_row_busn_trx_sp(
                                        p_business_id        => v_orig_busn_trx_tab(i).business_reference_id,
                                        p_business_sequence     => v_orig_busn_trx_tab(i).business_reference_seq,
                                        p_cur_busn_trx         => cur_ref_busn_trx
                                        );

                                IF cur_ref_busn_trx%ISOPEN
                                THEN
                                        FETCH cur_ref_busn_trx INTO v_ref_busn_trx_rec;
                                        LOOP
                                                IF cur_ref_busn_trx%NOTFOUND
                                                THEN
                                                        EXIT;
                                                END IF;
                                                -- buscamos el registro desde el cual se debe regenerar

                                                IF v_found_ref_busn_trx_flag
                                                OR (v_ref_busn_trx_rec.doc_portfolio_type    = v_orig_busn_trx_tab(i).doc_portfolio_type
                                                   AND v_ref_busn_trx_rec.doc_comm_location_code= v_orig_busn_trx_tab(i).doc_comm_location_code
                                                   AND v_ref_busn_trx_rec.document_year            = v_orig_busn_trx_tab(i).document_year
                                                   AND v_ref_busn_trx_rec.document_month        = v_orig_busn_trx_tab(i).document_month
                                                   AND v_ref_busn_trx_rec.doc_type_code         = v_orig_busn_trx_tab(i).doc_type_code
                                                   AND v_ref_busn_trx_rec.document_number       = v_orig_busn_trx_tab(i).document_number
                                                   AND v_ref_busn_trx_rec.document_item_num     = v_orig_busn_trx_tab(i).document_item_num
                                                   AND v_ref_busn_trx_rec.document_split_num    = v_orig_busn_trx_tab(i).document_split_num
                                                   )
                                                THEN
                                                        -- completamos los datos del registro padre
                                                        v_found_ref_busn_trx_flag:= TRUE;

                                                        v_busn_sequence:= v_busn_sequence + 1;

                                                        -- ajustamos los datos sobre la variable del registro
                                                        v_ref_busn_trx_rec.business_reference_id    := v_ref_busn_trx_rec.business_id;
                                                        v_ref_busn_trx_rec.business_reference_seq        := v_ref_busn_trx_rec.business_sequence;
                                                        v_ref_busn_trx_rec.business_id                := v_orig_busn_trx_tab(i).business_id;
                                                        v_ref_busn_trx_rec.business_sequence            := v_busn_sequence;

                                                        -- asignamos como padre al registro insertado anteriomente en la tabla plsql de la cadena nueva
                                                        v_ref_busn_trx_rec.parent_doc_comm_loc            := v_new_busn_trx_tab(j).doc_comm_location_code;
                                                        v_ref_busn_trx_rec.parent_doc_year            := v_new_busn_trx_tab(j).document_year;
                                                        v_ref_busn_trx_rec.parent_doc_portfolio_type    := v_new_busn_trx_tab(j).doc_portfolio_type;
                                                        v_ref_busn_trx_rec.parent_doc_month            := v_new_busn_trx_tab(j).document_month;
                                                        v_ref_busn_trx_rec.parent_doc_type_code            := v_new_busn_trx_tab(j).doc_type_code;
                                                        v_ref_busn_trx_rec.parent_doc_number            := v_new_busn_trx_tab(j).document_number;
                                                        v_ref_busn_trx_rec.parent_doc_item_num            := v_new_busn_trx_tab(j).document_item_num;
                                                        v_ref_busn_trx_rec.parent_doc_split_num            := v_new_busn_trx_tab(j).document_split_num;

                                                        -- los demas campos quedan tal cual para la copia

                                                        -- insertamos el registro a la tabla plsql de la cadena nueva
                                                        j:= j + 1;
                                                        v_new_busn_trx_tab(j):= v_ref_busn_trx_rec;
                                                END IF;

                                                FETCH cur_ref_busn_trx INTO v_ref_busn_trx_rec;
                                        END LOOP;

                                        CLOSE cur_ref_busn_trx;
                                END IF;

                                -- una vez copiada la cadena referenciada, salteamos la cadena orginal
                                WHILE i IS NOT NULL
                                AND v_orig_busn_trx_tab(i).business_reference_id IS NOT NULL
                                AND v_last_processed_busn_ref_id = v_orig_busn_trx_tab(i).business_reference_id
                                LOOP
                                        i:= v_orig_busn_trx_tab.NEXT(i);
                                END LOOP;

                        ELSE
                                -- sino copiamos directamente en la cadena nueva
                                -- modificamos la secuencia
                                v_busn_sequence:= v_busn_sequence + 1;

                                v_orig_busn_trx_tab(i).business_sequence:= v_busn_sequence;

                                -- insertamos en la tabla plsql de la cadena nueva
                                j:= j + 1;
                                v_new_busn_trx_tab(j):= v_orig_busn_trx_tab(i);
                                -- modificamos la dependencia

                                IF j > 1 THEN
                                        -- asignamos como padre al registro insertado anteriomente en la tabla plsql de la cadena nueva
                                        v_new_busn_trx_tab(j).parent_doc_comm_loc        := v_new_busn_trx_tab(j-1).doc_comm_location_code;
                                        v_new_busn_trx_tab(j).parent_doc_year            := v_new_busn_trx_tab(j-1).document_year;
                                        v_new_busn_trx_tab(j).parent_doc_portfolio_type := v_new_busn_trx_tab(j-1).doc_portfolio_type;
                                        v_new_busn_trx_tab(j).parent_doc_month            := v_new_busn_trx_tab(j-1).document_month;
                                        v_new_busn_trx_tab(j).parent_doc_type_code        := v_new_busn_trx_tab(j-1).doc_type_code;
                                        v_new_busn_trx_tab(j).parent_doc_number           := v_new_busn_trx_tab(j-1).document_number;
                                        v_new_busn_trx_tab(j).parent_doc_item_num    := v_new_busn_trx_tab(j-1).document_item_num;
                                        v_new_busn_trx_tab(j).parent_doc_split_num        := v_new_busn_trx_tab(j-1).document_split_num;
                                END IF;

                                i:= v_orig_busn_trx_tab.NEXT(i);
                        END IF;

                EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                                RAISE e_no_data_found_on_regen;
                END;
        END LOOP;

    -- una vez armada la cadena borramos los registros de la cadena original
        remove_busn_trx_by_bid_sp(
            p_business_id    => v_business_id
                );

    -- damos de alta los registros de la cadena nueva en la tabla
        insert_busn_trx_row_sp(
            p_busn_trx_tab    => v_new_busn_trx_tab
                );

EXCEPTION
        WHEN e_no_data_found_on_regen
        THEN
                -- buscamos el primer nodo de la cadena
                i:= p_busn_trx_tab.FIRST;

        -- almacenamos los datos del nodo padre root
                ods_busn_doc_tree_error_pkg.add_error_sp(
                        p_business_id                => p_busn_trx_tab(i).business_id,
                        p_parent_doc_comm_loc_code        => p_busn_trx_tab(i).parent_doc_comm_loc,
                        p_parent_doc_portfolio_type     => p_busn_trx_tab(i).parent_doc_portfolio_type,
                        p_parent_doc_year             => p_busn_trx_tab(i).parent_doc_year,
                        p_parent_doc_month             => LPAD(TRIM(p_busn_trx_tab(i).parent_doc_month),c_doc_month_length,'0'),
                        p_parent_doc_type_code             => p_busn_trx_tab(i).parent_doc_type_code,
                        p_parent_doc_num             => LPAD(TRIM(p_busn_trx_tab(i).parent_doc_number),c_doc_num_length,'0'),
                        p_parent_doc_item_num             => p_busn_trx_tab(i).parent_doc_item_num,
                        p_parent_doc_split_num             => p_busn_trx_tab(i).parent_doc_split_num,
                        p_error_desc                => c_no_data_found_on_regen_desc,
                        p_error_generation_date            => SYSDATE,
                        p_process_name                => c_ref_busn_trx_regen_process
                        );

                -- hacemos un rollback al punto al inicio del la regeneracion
                ROLLBACK TO sv_bef_ref_busn_trx_regen;
    WHEN others
          THEN
            IF cur_ref_busn_trx%ISOPEN
                THEN
                    CLOSE cur_ref_busn_trx;
                END IF;

                RAISE;
END regenerate_ref_busn_trx_sp;

--------------------------------------------------------------------------------

PROCEDURE proc_changed_ref_busn_trx_sp(
        p_busn_trx_bkp_tab    IN t_busn_trx_bkp_tab
        )
IS
        i PLS_INTEGER;
BEGIN
        -- recorremos la tabla de cadenas business transaction a actualizar
        i:= p_busn_trx_bkp_tab.FIRST;
        WHILE i IS NOT NULL
        LOOP

                -- si tiene registros ejecutamos la regeneracion
                IF p_busn_trx_bkp_tab(i).COUNT > 0
                THEN
                        regenerate_ref_busn_trx_sp(
                                p_busn_trx_tab => p_busn_trx_bkp_tab(i)
                                );
                END IF;

                -- recuperamos el siguiente registro
                i:= p_busn_trx_bkp_tab.NEXT(i);
        END LOOP;
END proc_changed_ref_busn_trx_sp;


--------------------------------------------------------------------------------

PROCEDURE get_last_busn_trx_node_sp(
        p_business_id           IN ods_business_transactions.business_id%TYPE,
        p_busn_trx_tab_row      OUT t_busn_trx_tab_row
        )
IS
BEGIN
        SELECT  *
        INTO    p_busn_trx_tab_row
        FROM    (
                SELECT  business_id,
                        business_sequence,
                        business_reference_id,
                        business_reference_seq,
                        doc_portfolio_type,
                        doc_comm_location_code,
                        document_year,
                        document_month,
                        doc_type_code,
                        document_number,
                        document_item_num,
                        document_split_num,
                        unlinked_doc_flag,
                        data_source_system,
                        parent_doc_comm_loc,
                        parent_doc_portfolio_type,
                        parent_doc_year,
                        parent_doc_month,
                        parent_doc_type_code,
                        parent_doc_number,
                        parent_doc_item_num,
                        parent_doc_split_num,
                        historical_link_flag,
                        secondary_costing_flag,
                        origin_business_id,
                        document_family_code,
                        DECODE(created_by, ods_common_pkg.c_man_data_source, ods_common_pkg.c_str_yes, ods_common_pkg.c_str_no) manual_record_flag,
                        integration_type,
                        created_date,
                        created_by,
                        last_updated_date,
                        last_updated_by,
                        deleted_date,
                        deleted_by
                FROM    ods_business_transactions
                WHERE   business_id = p_business_id
                ORDER BY business_sequence DESC
                )
        WHERE    ROWNUM = 1;

EXCEPTION
        -- si no se encuentra el registro se retorna el parametro en null.
        WHEN no_data_found
        THEN
                NULL;
END get_last_busn_trx_node_sp;

--------------------------------------------------------------------------------

PROCEDURE get_fam_busn_trx_child_nodes(
        p_parent_busn_trx_tab_row       IN t_busn_trx_tab_row,
        p_child_busn_trx_tab            OUT ods_business_transaction_pkg.t_family_code_busn_tab
        )
IS
        CURSOR cur_child_busn_trx(
                p_c_business_id         ods_business_transactions.business_id%TYPE,
                p_c_business_sequence   ods_business_transactions.business_sequence%TYPE
                )
        IS
        SELECT  d.business_id,
                d.business_sequence,
                d.document_family_code,
                NVL(c.family_ranking_order,c_max_family_ranking_order) family_ranking_order
        FROM    (
                SELECT  b.business_id,
                        b.business_sequence,
                        b.document_family_code
                FROM    ods_business_transactions a,
                        ods_business_transactions b
                WHERE   a.business_id = p_c_business_id
                AND     a.business_sequence = p_c_business_sequence
                AND     a.business_id = b.business_id
                AND     a.doc_portfolio_type = b.parent_doc_portfolio_type
                AND     a.doc_comm_location_code = b.parent_doc_comm_loc
                AND     a.document_year = b.parent_doc_year
                AND     a.document_month = b.parent_doc_month
                AND     a.doc_type_code = b.parent_doc_type_code
                AND     a.document_number = b.parent_doc_number
                AND     a.document_item_num = b.parent_doc_item_num
                AND     a.document_split_num = b.parent_doc_split_num
                ) d,
                ods_bsn_doc_tree_family_rank c
        WHERE   d.document_family_code = c.family_code (+)
        ORDER BY d.business_sequence;

        v_child_busn_trx                cur_child_busn_trx%ROWTYPE;
        v_family_ranking_first_value    NUMBER(10):= c_max_family_ranking_order + 1;
        --v_family_rank_first_busn_seq    ods_business_transactions.business_sequence%TYPE; ITTEN00367484

        i                               PLS_INTEGER:= 0;
       -- v_marked_flag                   BOOLEAN:= FALSE; ITTEN00367484
BEGIN
        --
        p_child_busn_trx_tab.DELETE;

        FOR v_child_busn_trx IN cur_child_busn_trx(
                                        p_c_business_id         => p_parent_busn_trx_tab_row.business_id,
                                        p_c_business_sequence   => p_parent_busn_trx_tab_row.business_sequence
                                        )
        LOOP

                i:= i + 1;
                -- recorremos agregando las business id a la tabla
                p_child_busn_trx_tab(i).business_id:= v_child_busn_trx.business_id;
                p_child_busn_trx_tab(i).business_sequence:= v_child_busn_trx.business_sequence;
                p_child_busn_trx_tab(i).document_family_code:= v_child_busn_trx.document_family_code;
                p_child_busn_trx_tab(i).family_ranking_order:= v_child_busn_trx.family_ranking_order;
                p_child_busn_trx_tab(i).primary_costing_flag:= ods_common_pkg.c_str_no;

                -- se verifica tambien el business_id del primero del ranking de familias
                IF v_child_busn_trx.family_ranking_order < v_family_ranking_first_value
                THEN
                        v_family_ranking_first_value:= v_child_busn_trx.family_ranking_order;
                      --  v_family_rank_first_busn_seq:= v_child_busn_trx.business_sequence; ITTEN00367484
                END IF;

        END LOOP;

        -- se marca el item de la tabla de business_id correspondiente al primero del ranking
        i:= p_child_busn_trx_tab.FIRST;
        --
        WHILE i IS NOT NULL --AND NOT v_marked_flag ITTEN00367484
        LOOP
                --
                IF p_child_busn_trx_tab(i).family_ranking_order = v_family_ranking_first_value --ITTEN00367484
                THEN
                        --
                        p_child_busn_trx_tab(i).primary_costing_flag:= ods_common_pkg.c_str_yes;
                        --v_marked_flag:= TRUE; ITTEN00367484
                        --
                END IF;
                --
                i:= p_child_busn_trx_tab.NEXT(i);
                --
        END LOOP;
        --
END  get_fam_busn_trx_child_nodes;


--------------------------------------------------------------------------------
PROCEDURE get_post_proc_node_sp (
        p_business_id           IN ods_business_transactions.business_id%TYPE,
        p_business_sequence     IN ods_business_transactions.business_sequence%TYPE,
        p_busn_trx_tab_row      OUT t_busn_trx_tab_row
        )
IS
BEGIN
        -- recuperamos el primer nodo con portfolio_type = '04' a partir de la
        -- secuencia especificada
        SELECT  a.*
        INTO    p_busn_trx_tab_row
        FROM    (
                SELECT  business_id                ,
                        business_sequence          ,
                        business_reference_id      ,
                        business_reference_seq     ,
                        doc_portfolio_type         ,
                        doc_comm_location_code     ,
                        document_year              ,
                        document_month             ,
                        doc_type_code              ,
                        document_number            ,
                        document_item_num          ,
                        document_split_num         ,
                        unlinked_doc_flag          ,
                        data_source_system         ,
                        parent_doc_comm_loc        ,
                        parent_doc_portfolio_type  ,
                        parent_doc_year            ,
                        parent_doc_month           ,
                        parent_doc_type_code       ,
                        parent_doc_number          ,
                        parent_doc_item_num        ,
                        parent_doc_split_num       ,
                        historical_link_flag       ,
                        secondary_costing_flag     ,
                        origin_business_id         ,
                        document_family_code       ,
                        DECODE(created_by,
                                        ods_common_pkg.c_man_data_source, ods_common_pkg.c_str_yes,
                                        ods_common_pkg.c_str_no
                                ) manual_record_flag,
                        integration_type           ,
                        created_date               ,
                        created_by                 ,
                        last_updated_date          ,
                        last_updated_by            ,
                        deleted_date               ,
                        deleted_by
                FROM    ods_business_transactions
                WHERE   business_id = p_business_id
                AND     doc_portfolio_type = c_post_process_portfolio_type
                AND     business_sequence >= p_business_sequence
                ORDER BY business_sequence
                ) a
        WHERE   ROWNUM = 1;

EXCEPTION
        -- si no se encuentra se retorna en NULL
        WHEN no_data_found
        THEN
                NULL;
END get_post_proc_node_sp;

--------------------------------------------------------------------------------

PROCEDURE mark_main_busn_id_cost_brch_sp(
        p_business_id           IN ods_business_transactions.business_id%TYPE,
        p_business_sequence     IN ods_business_transactions.business_sequence%TYPE
        )
IS
        v_busn_trx_tab_row      t_busn_trx_tab_row;
        v_child_busn_trx_tab    t_family_code_busn_tab;
        v_next_business_seq     ods_business_transactions.business_sequence%TYPE;

        i                PLS_INTEGER:= 0;
        j                PLS_INTEGER:= 0;
BEGIN
        -- verificamos si existe en business transaction un nodo con post proceso (portfolio_type '04')
        -- desde la sequencia indicada
        get_post_proc_node_sp (
                p_business_id           => p_business_id,
                p_business_sequence     => p_business_sequence,
                p_busn_trx_tab_row      => v_busn_trx_tab_row
                );
        --
        -- verificamos si se encontro un nodo
        IF v_busn_trx_tab_row.business_id IS NOT NULL
        THEN
                -- buscamos las posibles ramificaciones
                get_fam_busn_trx_child_nodes(
                        p_parent_busn_trx_tab_row       => v_busn_trx_tab_row,
                        p_child_busn_trx_tab            => v_child_busn_trx_tab
                        );
                --
                -- recorremos la tabla de nodos hijos
                i := v_child_busn_trx_tab.FIRST;
                --
                WHILE i IS NOT NULL
                LOOP
                        -- si no es la ramificacion primeria no se actualiza
                        IF v_child_busn_trx_tab(i).primary_costing_flag <> ods_common_pkg.c_str_yes
                        THEN
                                -- recuperamos el indice siguiente
                                j:= v_child_busn_trx_tab.NEXT(i);
                                --
                                IF j IS NOT NULL
                                THEN
                                        --
                                        v_next_business_seq:= v_child_busn_trx_tab(j).business_sequence;
                                        --
                                ELSE
                                        --
                                        v_next_business_seq:= NULL;
                                        --
                                END IF;
                                --
                                -- actualizamos la rama
                                UPDATE  ods_business_transactions
                                SET     secondary_costing_flag = ods_common_pkg.c_str_yes,
                                        last_updated_date = SYSDATE,
                                        last_updated_by = c_secondary_cost_process
                                WHERE   business_id = v_child_busn_trx_tab(i).business_id
                                AND     business_sequence >= v_child_busn_trx_tab(i).business_sequence
                                AND     business_sequence < NVL(v_next_business_seq,c_max_busn_trx_ref_seq);
                                --
                        END IF;
                        --
                        i:= v_child_busn_trx_tab.NEXT(i);
                        --
                END LOOP;
                --
        END IF;
        --
END mark_main_busn_id_cost_brch_sp;

--------------------------------------------------------------------------------

PROCEDURE mark_main_costing_branch_sp
IS
        --Recuperamos solo aquellos business_id generados por ONE
        --Solo recupera BIDs cuyo origen sea ONE
        CURSOR cur_busn_doc_tree_wrk
        IS
        SELECT  business_id
        FROM    ods_business_doc_tree_wrk
        WHERE   created_by = ods_common_pkg.c_one_data_source
        ORDER BY
                business_id;

        v_busn_doc_tree_wrk_rec         cur_busn_doc_tree_wrk%ROWTYPE;
        v_base_business_sequence        ods_business_transactions.business_sequence%TYPE;

BEGIN

        -- Recorremos el cursor ejecutando el proceso de determinacion de rama mas significativa para costos
        FOR v_busn_doc_tree_wrk_rec IN cur_busn_doc_tree_wrk
        LOOP
                -- seteamos la secuencia base para la búsqueda del nodo posproceso en 1
                v_base_business_sequence:= 1;

                -- procesamos un business id
                mark_main_busn_id_cost_brch_sp(
                        p_business_id           => v_busn_doc_tree_wrk_rec.business_id,
                        p_business_sequence     => v_base_business_sequence
                        );
        END LOOP;
        --
END mark_main_costing_branch_sp;


--------------------------------------------------------------------------------


PROCEDURE get_build_to_stock_busn_id_sp(
    p_doc_comm_location_code     IN ods_business_transactions.doc_comm_location_code%TYPE,
    p_document_year            IN ods_business_transactions.document_year%TYPE,
    p_document_month            IN ods_business_transactions.document_month%TYPE,
    p_doc_type_code                IN ods_business_transactions.doc_type_code%TYPE,
    p_document_number            IN ods_business_transactions.document_number%TYPE,
    p_document_item_num            IN ods_business_transactions.document_item_num%TYPE,
    p_document_split_num            IN ods_business_transactions.document_split_num%TYPE,
    p_business_trx_id_tab              OUT t_business_trx_id_tab
    )
IS
    CURSOR cur_business_trx_id
    IS
           SELECT    bt.business_id,
        bt.business_sequence
    FROM    ods_business_transactions bt
        WHERE   bt.doc_comm_location_code = p_doc_comm_location_code
        AND        bt.document_year = p_document_year
        AND        bt.document_month = p_document_month
        AND        bt.doc_type_code = p_doc_type_code
        AND        bt.document_number = p_document_number
        AND        bt.document_item_num = p_document_item_num
        AND        bt.document_split_num = p_document_split_num
    AND    bt.historical_link_flag = ods_common_pkg.c_str_no
    AND    EXISTS (
            SELECT    1
            FROM    ods_business_transactions br
            WHERE    br.business_id = bt.business_id
            AND    br.business_reference_id IS NOT NULL
        )
    ORDER BY business_id DESC;

    v_business_trx_id_rec t_business_trx_id;

    i PLS_INTEGER:= 0;
BEGIN
    -- Recorremos el cursor recuperando cada Business Id
    FOR v_business_trx_id_rec IN cur_business_trx_id
    LOOP
        -- almacenamos cada Business_id en la estructura p_business_trx_id_tab
        i := i + 1;
        p_business_trx_id_tab(i):= v_business_trx_id_rec;

    END LOOP;

END get_build_to_stock_busn_id_sp;

--------------------------------------------------------------------------------

PROCEDURE busn_doc_tree_man_to_trx_sp(
    p_busn_doc_tree_man_row     IN ods_business_doc_tree_man_pkg.t_doc_tree_man_tab_row,
    p_busn_trx_row              OUT t_busn_trx_tab_row
    )
IS
BEGIN
    -- completamos los datos del nodo parent y generales
    p_busn_trx_row.business_id        := p_busn_doc_tree_man_row.business_id                ;
    p_busn_trx_row.business_sequence          := c_max_busn_trx_seq;
    p_busn_trx_row.business_reference_id      := NULL;
    p_busn_trx_row.business_reference_seq     := NULL;
        p_busn_trx_row.parent_doc_comm_loc    := p_busn_doc_tree_man_row.parent_doc_comm_loc_code;
        p_busn_trx_row.parent_doc_portfolio_type:= p_busn_doc_tree_man_row.parent_doc_portfolio_type;
        p_busn_trx_row.parent_doc_year          := p_busn_doc_tree_man_row.parent_doc_year;
        p_busn_trx_row.parent_doc_month         := p_busn_doc_tree_man_row.parent_doc_month;
        p_busn_trx_row.parent_doc_type_code     := p_busn_doc_tree_man_row.parent_doc_type_code;
        p_busn_trx_row.parent_doc_number        := p_busn_doc_tree_man_row.parent_doc_num;
        p_busn_trx_row.parent_doc_item_num      := p_busn_doc_tree_man_row.parent_doc_item_num;
        p_busn_trx_row.parent_doc_split_num     := p_busn_doc_tree_man_row.parent_doc_split_num;

    p_busn_trx_row.unlinked_doc_flag    := NULL;
        p_busn_trx_row.data_source_system    := p_busn_doc_tree_man_row.data_source_system;
        p_busn_trx_row.historical_link_flag    := NULL;
    p_busn_trx_row.secondary_costing_flag    := NULL;
    p_busn_trx_row.origin_business_id    := NULL;
        p_busn_trx_row.document_family_code    := p_busn_doc_tree_man_row.doc_family_code;


        IF p_busn_doc_tree_man_row.data_source_system = ods_common_pkg.c_man_data_source
        THEN
        p_busn_trx_row.manual_record_flag    := ods_common_pkg.c_str_yes;
    ELSE
            p_busn_trx_row.manual_record_flag    := ods_common_pkg.c_str_no;
        END IF;

        p_busn_trx_row.integration_type        := p_busn_doc_tree_man_row.integration_type;
        p_busn_trx_row.created_date        := p_busn_doc_tree_man_row.created_date               ;
        p_busn_trx_row.created_by        := p_busn_doc_tree_man_row.created_by                 ;
        p_busn_trx_row.last_updated_date    := NULL;
        p_busn_trx_row.last_updated_by        := NULL;
        p_busn_trx_row.deleted_date        := NULL;
        p_busn_trx_row.deleted_by        := NULL;

    -- verificamos si se trata del caso Parent - Conferma
    IF p_busn_doc_tree_man_row.mill_ack_doc_comm_loc_code IS NOT NULL
    AND TRIM(p_busn_doc_tree_man_row.mill_ack_doc_num) IS NOT NULL
    AND p_busn_doc_tree_man_row.mill_ack_doc_num <> c_null_mill_ack_doc_num
        THEN
            -- se completan los datos del documento con los datos del nodo conferma
                p_busn_trx_row.doc_portfolio_type    := p_busn_doc_tree_man_row.doc_portfolio_type;
                p_busn_trx_row.doc_comm_location_code     := p_busn_doc_tree_man_row.mill_ack_doc_comm_loc_code;
                p_busn_trx_row.document_year              := p_busn_doc_tree_man_row.mill_ack_doc_year;
                p_busn_trx_row.document_month             := p_busn_doc_tree_man_row.mill_ack_doc_month;
                p_busn_trx_row.doc_type_code              := p_busn_doc_tree_man_row.mill_ack_doc_type_code;
                p_busn_trx_row.document_number            := p_busn_doc_tree_man_row.mill_ack_doc_num;
                p_busn_trx_row.document_item_num          := p_busn_doc_tree_man_row.mill_ack_item_num;
                p_busn_trx_row.document_split_num         := p_busn_doc_tree_man_row.mill_ack_split_num;
        ELSE
            -- se completan los datos del documento con los datos del nodo child
                p_busn_trx_row.doc_portfolio_type    := p_busn_doc_tree_man_row.doc_portfolio_type;
                p_busn_trx_row.doc_comm_location_code     := p_busn_doc_tree_man_row.doc_comm_loc_code;
                p_busn_trx_row.document_year              := p_busn_doc_tree_man_row.doc_year;
                p_busn_trx_row.document_month             := p_busn_doc_tree_man_row.doc_month;
                p_busn_trx_row.doc_type_code              := p_busn_doc_tree_man_row.doc_type_code;
                p_busn_trx_row.document_number            := p_busn_doc_tree_man_row.doc_num;
                p_busn_trx_row.document_item_num          := p_busn_doc_tree_man_row.doc_item_num;
                p_busn_trx_row.document_split_num         := p_busn_doc_tree_man_row.doc_split_num;
        END IF;


END busn_doc_tree_man_to_trx_sp;

--------------------------------------------------------------------------------

FUNCTION get_empty_busn_trx_row_cur_fn
RETURN ods_common_pkg.t_refcursor
IS
    v_cur_busn_trx ods_common_pkg.t_refcursor;
    v_busn_trx_result_table ods_busn_trx_table_type:= ods_busn_trx_table_type();

BEGIN
        -- generamos un registro para poder ejecutar la query sin registros
        v_busn_trx_result_table.EXTEND;
        v_busn_trx_result_table(v_busn_trx_result_table.COUNT) :=
            ods_busn_trx_row_type(
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL
                        );

        OPEN v_cur_busn_trx
        FOR
        SELECT    business_id            ,
                business_sequence              ,
                business_reference_id          ,
                business_reference_seq         ,
                doc_portfolio_type             ,
                doc_comm_location_code         ,
                document_year                  ,
                document_month                 ,
                doc_type_code                  ,
                document_number                ,
                document_item_num              ,
                document_split_num             ,
                unlinked_doc_flag              ,
                data_source_system             ,
                parent_doc_comm_loc            ,
                parent_doc_portfolio_type    ,
                parent_doc_year                  ,
                parent_doc_month                 ,
                parent_doc_type_code             ,
                parent_doc_number                ,
                parent_doc_item_num              ,
                parent_doc_split_num             ,
                historical_link_flag        ,
        secondary_costing_flag        ,
        origin_business_id        ,
                document_family_code        ,
        NULL    manual_record_flag    ,
                integration_type        ,
                created_date                      ,
                created_by                        ,
                last_updated_date                 ,
                last_updated_by                   ,
                deleted_date                      ,
                deleted_by
        FROM     TABLE(CAST(v_busn_trx_result_table AS ods_busn_trx_table_type))
        WHERE    1 = 0;

    RETURN  v_cur_busn_trx;
END get_empty_busn_trx_row_cur_fn;
--------------------------------------------------------------------------------

FUNCTION get_busn_trx_row_cur_fn(
    p_busn_trx_table    t_busn_trx_tab
        )
RETURN ods_common_pkg.t_refcursor
IS
    i PLS_INTEGER;

        v_cur_busn_trx t_refcursor;
        v_busn_trx_result_table ods_busn_trx_table_type:= ods_busn_trx_table_type();
BEGIN
    -- recorremos la tabla plsql y la transformamos en un object collection de tipo ods_busn_trx_table_type
    i:= p_busn_trx_table.FIRST;
        WHILE i IS NOT NULL
        LOOP
                -- generamos un registro para poder ejecutar la query sin registros
                v_busn_trx_result_table.EXTEND;
                v_busn_trx_result_table(v_busn_trx_result_table.COUNT) :=
                        ods_busn_trx_row_type(
                                p_busn_trx_table(i).business_id            ,
                                p_busn_trx_table(i).business_sequence          ,
                                p_busn_trx_table(i).business_reference_id      ,
                                p_busn_trx_table(i).business_reference_seq     ,
                                p_busn_trx_table(i).doc_portfolio_type         ,
                                p_busn_trx_table(i).doc_comm_location_code     ,
                                p_busn_trx_table(i).document_year              ,
                                p_busn_trx_table(i).document_month             ,
                                p_busn_trx_table(i).doc_type_code              ,
                                p_busn_trx_table(i).document_number            ,
                                p_busn_trx_table(i).document_item_num          ,
                                p_busn_trx_table(i).document_split_num         ,
                                p_busn_trx_table(i).unlinked_doc_flag          ,
                                p_busn_trx_table(i).data_source_system         ,
                                p_busn_trx_table(i).parent_doc_comm_loc        ,
                                p_busn_trx_table(i).parent_doc_portfolio_type    ,
                                p_busn_trx_table(i).parent_doc_year             ,
                                p_busn_trx_table(i).parent_doc_month            ,
                                p_busn_trx_table(i).parent_doc_type_code        ,
                                p_busn_trx_table(i).parent_doc_number           ,
                                p_busn_trx_table(i).parent_doc_item_num         ,
                                p_busn_trx_table(i).parent_doc_split_num        ,
                                p_busn_trx_table(i).historical_link_flag    ,
                                p_busn_trx_table(i).secondary_costing_flag    ,
                                p_busn_trx_table(i).origin_business_id        ,
                                p_busn_trx_table(i).document_family_code    ,
                                p_busn_trx_table(i).manual_record_flag        ,
                                p_busn_trx_table(i).integration_type        ,
                                p_busn_trx_table(i).created_date                ,
                                p_busn_trx_table(i).created_by                  ,
                                p_busn_trx_table(i).last_updated_date           ,
                                p_busn_trx_table(i).last_updated_by             ,
                                p_busn_trx_table(i).deleted_date                ,
                                p_busn_trx_table(i).deleted_by
                        );

        i:= p_busn_trx_table.NEXT(i);
        END LOOP;

        -- abrimos el cursor
        OPEN v_cur_busn_trx
        FOR
        SELECT    business_id            ,
                business_sequence              ,
                business_reference_id          ,
                business_reference_seq         ,
                doc_portfolio_type             ,
                doc_comm_location_code         ,
                document_year                  ,
                document_month                 ,
                doc_type_code                  ,
                document_number                ,
                document_item_num              ,
                document_split_num             ,
                unlinked_doc_flag              ,
                data_source_system             ,
                parent_doc_comm_loc            ,
                parent_doc_portfolio_type        ,
                parent_doc_year                  ,
                parent_doc_month                 ,
                parent_doc_type_code             ,
                parent_doc_number                ,
                parent_doc_item_num              ,
                parent_doc_split_num             ,
                historical_link_flag        ,
                secondary_costing_flag        ,
                origin_business_id        ,
                document_family_code        ,
                manual_record_flag        ,
                integration_type        ,
                created_date                      ,
                created_by                        ,
                last_updated_date                 ,
                last_updated_by                   ,
                deleted_date                      ,
                deleted_by
    FROM    TABLE(CAST(v_busn_trx_result_table AS ods_busn_trx_table_type))
        ORDER BY
            business_sequence;

        -- retornamos el ref cursor
        RETURN v_cur_busn_trx;

END get_busn_trx_row_cur_fn;

--------------------------------------------------------------------------------
FUNCTION find_busn_trx_node_seq_fn(
    p_business_id            IN ods_business_transactions.business_id%TYPE,
        p_doc_comm_location_code    IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_document_year            IN ods_business_transactions.document_year%TYPE,
        p_document_month            IN ods_business_transactions.document_month%TYPE,
        p_doc_type_code                IN ods_business_transactions.doc_type_code%TYPE,
        p_document_number            IN ods_business_transactions.document_number%TYPE,
        p_document_item_num            IN ods_business_transactions.document_item_num%TYPE,
        p_document_split_num            IN ods_business_transactions.document_split_num%TYPE
        )
RETURN ods_business_transactions.business_sequence%TYPE
IS
        v_result     BOOLEAN:= TRUE;
        v_business_sequence    ods_business_transactions.business_sequence%TYPE;
BEGIN
        BEGIN
            SELECT    business_sequence
                INTO    v_business_sequence
                FROM    ods_business_transactions
                WHERE   doc_comm_location_code = p_doc_comm_location_code
                AND        document_year = p_document_year
                AND        document_month = p_document_month
                AND        doc_type_code = p_doc_type_code
                AND        document_number = p_document_number
                AND        document_item_num = p_document_item_num
                AND        document_split_num = p_document_split_num
                AND    business_id = p_business_id
                AND        ROWNUM = 1;
        EXCEPTION
    WHEN no_data_found
    THEN
            v_business_sequence:= NULL;
        END;

        RETURN v_business_sequence;
END find_busn_trx_node_seq_fn;

--------------------------------------------------------------------------------
PROCEDURE generate_parent_busn_trx_sp(
    p_busn_doc_tree_man_row     IN ods_business_doc_tree_man_pkg.t_doc_tree_man_tab_row,
    p_busn_trx_row              OUT t_busn_trx_tab_row
    )
IS
BEGIN
    -- completamos los datos del nodo parent y generales
    p_busn_trx_row.business_id        := p_busn_doc_tree_man_row.business_id                ;
    p_busn_trx_row.business_sequence          := c_max_busn_trx_seq;
    p_busn_trx_row.business_reference_id      := NULL;
    p_busn_trx_row.business_reference_seq     := NULL;
        p_busn_trx_row.parent_doc_comm_loc    := NULL;
        p_busn_trx_row.parent_doc_portfolio_type:= NULL;
        p_busn_trx_row.parent_doc_year          := NULL;
        p_busn_trx_row.parent_doc_month         := NULL;
        p_busn_trx_row.parent_doc_type_code     := NULL;
        p_busn_trx_row.parent_doc_number        := NULL;
        p_busn_trx_row.parent_doc_item_num      := NULL;
        p_busn_trx_row.parent_doc_split_num     := NULL;

    p_busn_trx_row.unlinked_doc_flag    := NULL;
        p_busn_trx_row.data_source_system    := p_busn_doc_tree_man_row.data_source_system;
        p_busn_trx_row.historical_link_flag    := NULL;
    p_busn_trx_row.secondary_costing_flag    := NULL;
    p_busn_trx_row.origin_business_id    := NULL;
        p_busn_trx_row.document_family_code    := p_busn_doc_tree_man_row.doc_family_code;
    p_busn_trx_row.manual_record_flag    := ods_common_pkg.c_str_yes;

        p_busn_trx_row.integration_type        := p_busn_doc_tree_man_row.integration_type;
        p_busn_trx_row.created_date        := p_busn_doc_tree_man_row.created_date               ;
        p_busn_trx_row.created_by        := p_busn_doc_tree_man_row.created_by                 ;
        p_busn_trx_row.last_updated_date    := NULL;
        p_busn_trx_row.last_updated_by        := NULL;
        p_busn_trx_row.deleted_date        := NULL;
        p_busn_trx_row.deleted_by        := NULL;


        -- se completan los datos del documento con los datos del documento con los datos del parent
        p_busn_trx_row.doc_portfolio_type    := p_busn_doc_tree_man_row.parent_doc_portfolio_type;
        p_busn_trx_row.doc_comm_location_code     := p_busn_doc_tree_man_row.parent_doc_comm_loc_code;
        p_busn_trx_row.document_year              := p_busn_doc_tree_man_row.parent_doc_year;
        p_busn_trx_row.document_month             := p_busn_doc_tree_man_row.parent_doc_month;
        p_busn_trx_row.doc_type_code              := p_busn_doc_tree_man_row.parent_doc_type_code;
        p_busn_trx_row.document_number            := p_busn_doc_tree_man_row.parent_doc_num;
        p_busn_trx_row.document_item_num          := p_busn_doc_tree_man_row.parent_doc_item_num;
        p_busn_trx_row.document_split_num         := p_busn_doc_tree_man_row.parent_doc_split_num;
    p_busn_trx_row.document_family_code    := p_busn_doc_tree_man_row.parent_family_code;


END generate_parent_busn_trx_sp;

--------------------------------------------------------------------------------
PROCEDURE delete_error_busn_id_sp
IS
        v_error_busn_id_table    ods_busn_doc_tree_error_pkg.t_error_busn_id_table;
        i PLS_INTEGER;

BEGIN
    -- recuperamos los business_id que dieron error en el procesamiento y filtramos el error c_busn_trx_ref_no_found_desc
    ---(cadenas que no encuentran referencia) ITTEN00367484
        ods_busn_doc_tree_error_pkg.retrieve_error_busn_id_list_sp (
                p_error_busn_id_table     => v_error_busn_id_table
                ,p_exception_value        => c_busn_trx_ref_no_found_desc
                );

    -- recorremos borrando los mismos
        i:=  v_error_busn_id_table.FIRST;

        WHILE i IS NOT NULL
        LOOP
                ods_business_doc_tree_pkg.delete_busn_doc_tree_wrk_sp(
                        p_business_id    => v_error_busn_id_table(i)
                        );

        i:=  v_error_busn_id_table.NEXT(i);
        END LOOP;

END delete_error_busn_id_sp;

--------------------------------------------------------------------------------
FUNCTION invalid_parent_millack_node_fn(
        p_busn_doc_tree_tab_row IN ods_business_doc_tree_pkg.t_doc_tree_node_tab_row
        )
RETURN BOOLEAN
IS
        v_result BOOLEAN:= FALSE;
BEGIN
        -- verificamos si se trata de un nodo parent mill_ack (se verifica si el nodo child es nulo)
        IF      p_busn_doc_tree_tab_row.doc_comm_loc_code IS NULL
          OR    TRIM(p_busn_doc_tree_tab_row.doc_item_num) IS NULL
        THEN
                -- verificamos si se trata de un nodo conferma de matalmecanica y el numero de documento del conferma es '00000000'
                IF      p_busn_doc_tree_tab_row.mill_ack_doc_comm_loc_code = ods_common_pkg.c_metalmecanica_comm_loc
                   AND (   p_busn_doc_tree_tab_row.mill_ack_doc_num = ods_common_pkg.c_invalid_millack_doc_num
                        OR TRIM(p_busn_doc_tree_tab_row.mill_ack_doc_num ) IS NULL
                       )
                THEN
                        v_result:= TRUE;
                END IF;
        END IF;

        RETURN v_result;
END invalid_parent_millack_node_fn;

----------------------------------------------------------------------------------------------

PROCEDURE update_bt_serv_sale_sp(
        p_ss_comm_loc_code      IN ods_bt_service_sales.ss_comm_loc_code%TYPE,
        p_ss_order_year         IN ods_bt_service_sales.ss_order_year%TYPE,
        p_ss_order_month        IN ods_bt_service_sales.ss_order_month%TYPE,
        p_ss_doc_type_code      IN ods_bt_service_sales.ss_doc_type_code%TYPE,
        p_ss_number             IN ods_bt_service_sales.ss_number%TYPE,
        p_ss_item_num           IN ods_bt_service_sales.ss_item_num%TYPE,
        p_ss_split_num          IN ods_bt_service_sales.ss_split_num%TYPE,
        p_business_id           IN ods_bt_service_sales.business_id%TYPE
        )
IS
BEGIN
        UPDATE  ods_bt_service_sales
        SET     business_id = p_business_id,
                process_date = SYSDATE
        WHERE   ss_comm_loc_code = p_ss_comm_loc_code
        AND     ss_order_year = p_ss_order_year
        AND     ss_order_month = p_ss_order_month
        AND     ss_doc_type_code = p_ss_doc_type_code
        AND     ss_number = p_ss_number
        AND     ss_item_num = p_ss_item_num
        AND     ss_split_num = p_ss_split_num;

END update_bt_serv_sale_sp;


----------------------------------------------------------------------------------------------

PROCEDURE generate_doc_bt_serv_sale_sp(
        p_bt_service_sale_rec    IN t_bt_service_sale_rec
        )
IS
    	v_cur_busn_trx                  ods_common_pkg.t_refcursor;
    	v_busn_trx_tab_row              t_busn_trx_tab_row;
        v_base_node_found_flag          BOOLEAN:= FALSE;
        v_max_busn_sequence             ods_business_transactions.business_sequence%TYPE:= 0;

        v_bt_ss_parent_busn_trx_rec     t_busn_trx_tab_row;
        v_new_busn_trx_rec              t_busn_trx_tab_row;
        v_process_date                  DATE:= SYSDATE;
BEGIN
        -- buscamos la cadena a la que pertenece la orden del service sale
        get_business_transaction_sp(
                p_doc_comm_loc_code     => p_bt_service_sale_rec.so_comm_loc_code,
                p_document_year         => p_bt_service_sale_rec.so_order_year   ,
                p_document_month        => p_bt_service_sale_rec.so_order_month  ,
                p_doc_type_code         => p_bt_service_sale_rec.so_doc_type_code,
                p_document_number       => p_bt_service_sale_rec.so_number       ,
                p_document_item_num     => p_bt_service_sale_rec.so_item_num     ,
                p_document_split_num    => p_bt_service_sale_rec.so_split_num    ,
                p_include_manual_flag   => ods_common_pkg.c_str_false,
                p_chain_type            => ods_common_pkg.c_build_to_order_type,
                p_cur_busn_trx          => v_cur_busn_trx
                );

        -- verificamos si se encontro la cadena
        FETCH v_cur_busn_trx INTO v_busn_trx_tab_row;
        --
        WHILE v_cur_busn_trx%FOUND
        LOOP
            	-- si se encontro la cadena se busca dentro del mismo el primer nodo que
                -- coincida en commercial location y que sea portfolio 04 (post proceso)
                IF NOT v_base_node_found_flag
                   --Se comenta la condición que hacía coincidir la Commercial Location, para evitar que se
                   --genere la cadena sin el Nodo de Servicio.
                   --AND v_busn_trx_tab_row.doc_comm_location_code = p_bt_service_sale_rec.ss_comm_loc_code
                   AND v_busn_trx_tab_row.doc_portfolio_type = c_post_process_portfolio_type
                THEN
                        -- se almacena los datos del nodo para poder luego encadenar el nodo nuevo a este encontrado
                        v_bt_ss_parent_busn_trx_rec:= v_busn_trx_tab_row;
                        --
                        v_base_node_found_flag:= TRUE;
                        --
                END IF;
                --
                IF v_busn_trx_tab_row.business_sequence > v_max_busn_sequence
                THEN
                        --
                        v_max_busn_sequence:= v_busn_trx_tab_row.business_sequence;
                        --
                END IF;
                --
                FETCH v_cur_busn_trx INTO v_busn_trx_tab_row;
                --
        END LOOP;
        --
        IF v_bt_ss_parent_busn_trx_rec.business_id IS NOT NULL
        THEN

                -- armamos el nodo nuevo
                v_new_busn_trx_rec.business_id                  := v_bt_ss_parent_busn_trx_rec.business_id;
                v_new_busn_trx_rec.business_sequence            := v_max_busn_sequence + 1;
                v_new_busn_trx_rec.business_reference_id        := NULL;
                v_new_busn_trx_rec.business_reference_seq       := NULL;
                v_new_busn_trx_rec.doc_portfolio_type           := c_sale_portfolio_type;
                v_new_busn_trx_rec.doc_comm_location_code       := p_bt_service_sale_rec.ss_comm_loc_code;
                v_new_busn_trx_rec.document_year                := p_bt_service_sale_rec.ss_order_year;
                v_new_busn_trx_rec.document_month               := LPAD(TRIM(p_bt_service_sale_rec.ss_order_month),c_doc_month_length,'0');
                v_new_busn_trx_rec.doc_type_code                := p_bt_service_sale_rec.ss_doc_type_code;
                v_new_busn_trx_rec.document_number              := LPAD(TRIM(p_bt_service_sale_rec.ss_number),c_doc_num_length,'0');
                v_new_busn_trx_rec.document_item_num            := p_bt_service_sale_rec.ss_item_num;
                v_new_busn_trx_rec.document_split_num           := p_bt_service_sale_rec.ss_split_num;
                v_new_busn_trx_rec.data_source_system           := ods_common_pkg.c_doc_tree_one_data_source;
                v_new_busn_trx_rec.parent_doc_comm_loc          := v_bt_ss_parent_busn_trx_rec.doc_comm_location_code;
                v_new_busn_trx_rec.parent_doc_portfolio_type    := v_bt_ss_parent_busn_trx_rec.doc_portfolio_type    ;
                v_new_busn_trx_rec.parent_doc_year              := v_bt_ss_parent_busn_trx_rec.document_year         ;
                v_new_busn_trx_rec.parent_doc_month             := v_bt_ss_parent_busn_trx_rec.document_month        ;
                v_new_busn_trx_rec.parent_doc_type_code         := v_bt_ss_parent_busn_trx_rec.doc_type_code         ;
                v_new_busn_trx_rec.parent_doc_number            := v_bt_ss_parent_busn_trx_rec.document_number       ;
                v_new_busn_trx_rec.parent_doc_item_num          := v_bt_ss_parent_busn_trx_rec.document_item_num     ;
                v_new_busn_trx_rec.parent_doc_split_num         := v_bt_ss_parent_busn_trx_rec.document_split_num    ;
                v_new_busn_trx_rec.unlinked_doc_flag            := ods_common_pkg.c_str_no;
                v_new_busn_trx_rec.historical_link_flag         := ods_common_pkg.c_str_no;
                v_new_busn_trx_rec.secondary_costing_flag       := ods_common_pkg.c_str_yes;
                v_new_busn_trx_rec.origin_business_id           := NULL;
                --
                --v_new_busn_trx_rec.document_family_code         := NULL;
                v_new_busn_trx_rec.document_family_code         := p_bt_service_sale_rec.ss_family_code;
                --
                v_new_busn_trx_rec.integration_type             := NULL;
                v_new_busn_trx_rec.created_date                 := v_process_date;
                v_new_busn_trx_rec.created_by                   := c_bt_serv_sale_process;
                v_new_busn_trx_rec.last_updated_date            := v_process_date;
                v_new_busn_trx_rec.last_updated_by              := c_bt_serv_sale_process;
                v_new_busn_trx_rec.deleted_date                 := NULL;
                v_new_busn_trx_rec.deleted_by                   := NULL;

                -- damos de alta el nodo nuevo
                insert_busn_trx_row_sp(
                        p_insert_busn_trx_tab_row    => v_new_busn_trx_rec
                        );

                -- actualizamos el bt service sale correspondiente con el business_id
                update_bt_serv_sale_sp(
                        p_ss_comm_loc_code      => p_bt_service_sale_rec.ss_comm_loc_code,
                        p_ss_order_year         => p_bt_service_sale_rec.ss_order_year   ,
                        p_ss_order_month        => p_bt_service_sale_rec.ss_order_month  ,
                        p_ss_doc_type_code      => p_bt_service_sale_rec.ss_doc_type_code,
                        p_ss_number             => p_bt_service_sale_rec.ss_number       ,
                        p_ss_item_num           => p_bt_service_sale_rec.ss_item_num     ,
                        p_ss_split_num          => p_bt_service_sale_rec.ss_split_num    ,
                        p_business_id           => v_bt_ss_parent_busn_trx_rec.business_id
                        );

                -- damos de alta el business_id correspondiente e la tabla ods_business_doc_tree_wrk
                ods_business_doc_tree_pkg.insert_busn_doc_tree_wrk_sp(
                        p_business_id   => v_bt_ss_parent_busn_trx_rec.business_id,
                        p_created_by    => ods_common_pkg.c_one_data_source
                        );
                --
        END IF;
        --
END generate_doc_bt_serv_sale_sp;

----------------------------------------------------------------------------------------------

PROCEDURE generate_bt_serv_sale_sp
IS
        CURSOR cur_bt_serv_sales
        IS
        SELECT  ss_comm_loc_code,
                ss_order_year,
                ss_order_month,
                ss_doc_type_code,
                ss_number,
                ss_item_num,
                ss_split_num,
                --
                ss_family_code,
                --
                business_id,
                so_comm_loc_code,
                so_order_year,
                so_order_month,
                so_doc_type_code,
                so_number,
                so_item_num,
                so_split_num
        FROM    ods_bt_service_sales
        WHERE   business_id = ods_common_pkg.c_invalid_business_id
        UNION ALL
        SELECT  ss.ss_comm_loc_code,
                ss.ss_order_year,
                ss.ss_order_month,
                ss.ss_doc_type_code,
                ss.ss_number,
                ss.ss_item_num,
                ss.ss_split_num,
                --
                ss.ss_family_code,
                --
                ss.business_id,
                ss.so_comm_loc_code,
                ss.so_order_year,
                ss.so_order_month,
                ss.so_doc_type_code,
                ss.so_number,
                ss.so_item_num,
                ss.so_split_num
        FROM    ods_bt_service_sales ss,
                ods_business_doc_tree_wrk biw
        WHERE   ss.business_id = biw.business_id
        AND     ss.business_id <> ods_common_pkg.c_invalid_business_id;

        v_bt_serv_sale_rec t_bt_service_sale_rec;
BEGIN
        FOR v_bt_serv_sale_rec IN cur_bt_serv_sales
        LOOP
                -- por cada registro ejecutamos la generacion del nodo service sale
                generate_doc_bt_serv_sale_sp(
                        p_bt_service_sale_rec    => v_bt_serv_sale_rec
                        );

        END LOOP;
END generate_bt_serv_sale_sp;

----------------------------------------------------------------------------------------------

/*PROCEDURE generate_doc_g17_conf_order_sp(
        p_g17_business_id    IN ods_business_transactions.BUSINESS_ID%type
    )
IS
    	v_cur_busn_trx         		ods_common_pkg.t_refcursor;
    	v_busn_trx_tab_row    		t_busn_trx_tab_row;
        v_base_node_found_flag  	BOOLEAN:= FALSE;
        v_sale_node_found_flag  	BOOLEAN:= FALSE;
        v_max_busn_sequence     	ods_business_transactions.business_sequence%TYPE:= 0;

        v_g17_sales_busn_trx_rec        t_busn_trx_tab_row;
        v_g17_parent_busn_trx_rec       t_busn_trx_tab_row;
        v_g17_new_busn_trx_rec          t_busn_trx_tab_row;

        v_bt_ss_parent_busn_trx_rec     t_busn_trx_tab_row;
        v_new_busn_trx_rec      	t_busn_trx_tab_row;
        v_process_date          	DATE:= SYSDATE;
BEGIN
        -- recuperamos la cadena
        get_business_transaction_sp(
                p_business_id        	=> p_g17_business_id,
                p_include_manual_flag   => ods_common_pkg.c_str_false,
                p_cur_busn_trx        	=> v_cur_busn_trx
                );

        -- verificamos si se encontro la cadena
        FETCH v_cur_busn_trx INTO v_busn_trx_tab_row;

        WHILE v_cur_busn_trx%FOUND
        LOOP
                -- Si se encontró la cadena, se busca dentro del mismo el primer nodo que
                -- coincida en Commercial Location = G17 y que sea portfolio 02 (Venta)
                IF NOT v_sale_node_found_flag
                   AND v_busn_trx_tab_row.doc_comm_location_code = ods_common_pkg.c_metalmecanica_comm_loc
                   AND v_busn_trx_tab_row.doc_portfolio_type = c_sale_portfolio_type
                THEN
                        -- Se almacenan los datos del nodo de Ventas para poder luego utilizarlo
                        v_g17_sales_busn_trx_rec := v_busn_trx_tab_row;
                        v_sale_node_found_flag := TRUE;
                END IF;

                --El nuevo nodo se insertará luego del último encontrado con Commercial Location = G17
                --Los datos de este registro serán los valores para los campos "PARENT" del nuevo registro que se generará
                IF   v_busn_trx_tab_row.doc_comm_location_code = ods_common_pkg.c_metalmecanica_comm_loc
                 AND v_busn_trx_tab_row.business_sequence > v_max_busn_sequence
                THEN
                        v_g17_parent_busn_trx_rec := v_busn_trx_tab_row;
                        v_max_busn_sequence := v_busn_trx_tab_row.business_sequence;
                END IF;

                FETCH v_cur_busn_trx INTO v_busn_trx_tab_row;

        END LOOP;

        IF   v_g17_sales_busn_trx_rec.business_id IS NOT NULL
   	 and v_g17_parent_busn_trx_rec.business_id IS NOT NULL
        THEN
                -- armamos el nodo nuevo
                v_new_busn_trx_rec.business_id                  := v_g17_sales_busn_trx_rec.business_id;
                v_new_busn_trx_rec.business_sequence            := v_max_busn_sequence + 1;
                v_new_busn_trx_rec.business_reference_id        := v_g17_parent_busn_trx_rec.business_reference_id;
                v_new_busn_trx_rec.business_reference_seq       := case
                                                                        when v_g17_parent_busn_trx_rec.business_reference_seq = 99999999
                							then v_g17_parent_busn_trx_rec.business_reference_seq
                							else v_g17_parent_busn_trx_rec.business_reference_seq + 1
                                                                   end;
                v_new_busn_trx_rec.doc_portfolio_type           := c_mill_ack_portfolio_type;
                v_new_busn_trx_rec.doc_comm_location_code       := v_g17_sales_busn_trx_rec.doc_comm_location_code;
                v_new_busn_trx_rec.document_year                := v_g17_sales_busn_trx_rec.document_year;
                v_new_busn_trx_rec.document_month               := v_g17_sales_busn_trx_rec.document_month;
                v_new_busn_trx_rec.doc_type_code                := c_null_ack_doc_type;
                v_new_busn_trx_rec.document_number              := v_g17_sales_busn_trx_rec.document_number;
                v_new_busn_trx_rec.document_item_num            := v_g17_sales_busn_trx_rec.document_item_num;
                v_new_busn_trx_rec.document_split_num           := v_g17_sales_busn_trx_rec.document_split_num;
                v_new_busn_trx_rec.data_source_system           := v_g17_parent_busn_trx_rec.data_source_system;
                v_new_busn_trx_rec.parent_doc_comm_loc          := v_g17_parent_busn_trx_rec.doc_comm_location_code;
                v_new_busn_trx_rec.parent_doc_portfolio_type    := v_g17_parent_busn_trx_rec.doc_portfolio_type;
                v_new_busn_trx_rec.parent_doc_year              := v_g17_parent_busn_trx_rec.document_year         ;
                v_new_busn_trx_rec.parent_doc_month             := v_g17_parent_busn_trx_rec.document_month        ;
                v_new_busn_trx_rec.parent_doc_type_code         := v_g17_parent_busn_trx_rec.doc_type_code         ;
                v_new_busn_trx_rec.parent_doc_number            := v_g17_parent_busn_trx_rec.document_number       ;
                v_new_busn_trx_rec.parent_doc_item_num          := v_g17_parent_busn_trx_rec.document_item_num     ;
                v_new_busn_trx_rec.parent_doc_split_num         := v_g17_parent_busn_trx_rec.document_split_num    ;
                v_new_busn_trx_rec.unlinked_doc_flag            := 'N';
                v_new_busn_trx_rec.historical_link_flag         := 'N';
                v_new_busn_trx_rec.secondary_costing_flag       := v_g17_parent_busn_trx_rec.secondary_costing_flag;
                --v_new_busn_trx_rec.origin_business_id       := NULL;
                --v_new_busn_trx_rec.document_family_code     := NULL;
                --v_new_busn_trx_rec.integration_type         := NULL;
                --
                --v_new_busn_trx_rec.document_family_code         := NVL(v_g17_sales_busn_trx_rec.document_family_code,v_g17_parent_busn_trx_rec.document_family_code);
                v_new_busn_trx_rec.document_family_code         := v_g17_sales_busn_trx_rec.document_family_code;
                --
                v_new_busn_trx_rec.created_date                 := v_process_date;
                v_new_busn_trx_rec.created_by                   := v_g17_parent_busn_trx_rec.created_by;
                v_new_busn_trx_rec.last_updated_date            := v_process_date;
                v_new_busn_trx_rec.last_updated_by              := v_g17_parent_busn_trx_rec.created_by;
                v_new_busn_trx_rec.deleted_date                 := null;
                v_new_busn_trx_rec.deleted_by                   := null;

                -- damos de alta el nodo nuevo
                insert_busn_trx_row_sp(
                        p_insert_busn_trx_tab_row    => v_new_busn_trx_rec
                        );

        END IF;

END generate_doc_g17_conf_order_sp;*/

PROCEDURE generate_doc_g17_conf_order_sp(
        p_g17_business_id    IN ods_business_transactions.BUSINESS_ID%type
    )
IS
      v_cur_busn_trx             ods_common_pkg.t_refcursor;
      v_busn_trx_tab_row         t_busn_trx_tab_row;

      v_g17_sales_busn_trx_rec   t_busn_trx_tab_row;
      v_g17_parent_busn_trx_rec  t_busn_trx_tab_row;

      v_new_busn_trx_rec         t_busn_trx_tab_row;

      v_sale_node_found_flag     BOOLEAN:= FALSE;
      v_max_busn_sequence        ods_business_transactions.business_sequence%TYPE:= 0;
      v_corrimiento              PLS_INTEGER:=0;
BEGIN
        -- recuperamos la cadena desde la business transactions
        get_business_transaction_sp(
                p_business_id        	=> p_g17_business_id,
                p_include_manual_flag   => ods_common_pkg.c_str_false,
                p_cur_busn_trx        	=> v_cur_busn_trx
                );

        -- verificamos si se encontró la cadena
        FETCH v_cur_busn_trx INTO v_busn_trx_tab_row;

        WHILE v_cur_busn_trx%FOUND
        LOOP
                -- Si no se encontró un G17 (v_sale_node_found_flag) y el documento coincide en Commercial Location = G17 y que sea portfolio 02 (Venta)
                IF NOT v_sale_node_found_flag
                   AND v_busn_trx_tab_row.doc_comm_location_code = ods_common_pkg.c_metalmecanica_comm_loc
                   AND v_busn_trx_tab_row.doc_portfolio_type = c_sale_portfolio_type
                THEN
                        -- Se almacenan los datos del nodo de Ventas para poder luego insertarlo como 88
                        v_g17_sales_busn_trx_rec := v_busn_trx_tab_row;
                        v_sale_node_found_flag   := TRUE; --Determino que ya encontré el G17
                END IF;

                --Si se encontró un G17 (v_sale_node_found_flag) Significa que aún sigo en los documentos g17
                --El nuevo nodo se insertará luego del último encontrado con Commercial Location = G17
                --Los datos de este registro serán los valores para los campos "PARENT" del nuevo registro que se generará
                IF  v_sale_node_found_flag THEN
                        v_g17_parent_busn_trx_rec := v_busn_trx_tab_row;
                        v_max_busn_sequence       := v_busn_trx_tab_row.business_sequence + v_corrimiento;
                        --(Guardo la secuencia del ultimo G17 encontrado al momento)
                        --le sumo el corrimiento por si ya se insertó en la tabla ods_business_transactions algún registro.
                END IF;

                -- Si se encontró un G17 (v_sale_node_found_flag) y estoy parado en un 88 no se precisa insertar nada
                --por lo tanto Determino que ya no encontré el G17
                IF  v_sale_node_found_flag
                    AND
                    v_busn_trx_tab_row.doc_portfolio_type = c_mill_ack_portfolio_type
                THEN
                        v_sale_node_found_flag   := FALSE;
                END IF;

                --paso al próximo registro.(para determinar si es el último o si cambio de sociedad y no es mas G17)
                FETCH v_cur_busn_trx INTO v_busn_trx_tab_row;

                -- Si se encontró un G17 (v_sale_node_found_flag) y es el último registro
                -- o el registro no es más un G17, realizo el insert del 88
                IF  v_sale_node_found_flag
                    AND
                    ( v_cur_busn_trx%NOTFOUND
                    OR
                      v_busn_trx_tab_row.doc_comm_location_code <> ods_common_pkg.c_metalmecanica_comm_loc
                    )
                THEN
                          --Corro todas las secuencias +1 así deja espacio para insertar el 88
                          UPDATE ODS_BUSINESS_TRANSACTIONS
                          SET BUSINESS_SEQUENCE = BUSINESS_SEQUENCE + 1
                          WHERE 1=1
                          AND BUSINESS_ID       = V_G17_SALES_BUSN_TRX_REC.BUSINESS_ID
                          AND BUSINESS_SEQUENCE > V_MAX_BUSN_SEQUENCE;

                          --como corrí todo una secuencia más (con el update)
                          --incremento en 1 a v_corrimiento
                          v_corrimiento:= v_corrimiento+1;

                          --Preparo el registro a insertar
                          v_new_busn_trx_rec.business_id                  := v_g17_sales_busn_trx_rec.business_id;
                          v_new_busn_trx_rec.business_sequence            := v_max_busn_sequence + 1;
                          v_new_busn_trx_rec.business_reference_id        := v_g17_parent_busn_trx_rec.business_reference_id;
                          v_new_busn_trx_rec.business_reference_seq       := case
                                                                                 when v_g17_parent_busn_trx_rec.business_reference_seq = 99999999
                							                                                   then v_g17_parent_busn_trx_rec.business_reference_seq
                							                                                   else v_g17_parent_busn_trx_rec.business_reference_seq + 1
                                                                             end;
                          v_new_busn_trx_rec.doc_portfolio_type           := c_mill_ack_portfolio_type;
                          v_new_busn_trx_rec.doc_comm_location_code       := v_g17_sales_busn_trx_rec.doc_comm_location_code;
                          v_new_busn_trx_rec.document_year                := v_g17_sales_busn_trx_rec.document_year;
                          v_new_busn_trx_rec.document_month               := v_g17_sales_busn_trx_rec.document_month;
                          v_new_busn_trx_rec.doc_type_code                := c_null_ack_doc_type;
                          v_new_busn_trx_rec.document_number              := v_g17_sales_busn_trx_rec.document_number;
                          v_new_busn_trx_rec.document_item_num            := v_g17_sales_busn_trx_rec.document_item_num;
                          v_new_busn_trx_rec.document_split_num           := v_g17_sales_busn_trx_rec.document_split_num;

                          v_new_busn_trx_rec.data_source_system           := v_g17_parent_busn_trx_rec.data_source_system;
                          v_new_busn_trx_rec.parent_doc_comm_loc          := v_g17_parent_busn_trx_rec.doc_comm_location_code;
                          v_new_busn_trx_rec.parent_doc_portfolio_type    := v_g17_parent_busn_trx_rec.doc_portfolio_type;
                          v_new_busn_trx_rec.parent_doc_year              := v_g17_parent_busn_trx_rec.document_year;
                          v_new_busn_trx_rec.parent_doc_month             := v_g17_parent_busn_trx_rec.document_month;
                          v_new_busn_trx_rec.parent_doc_type_code         := v_g17_parent_busn_trx_rec.doc_type_code;
                          v_new_busn_trx_rec.parent_doc_number            := v_g17_parent_busn_trx_rec.document_number;
                          v_new_busn_trx_rec.parent_doc_item_num          := v_g17_parent_busn_trx_rec.document_item_num;
                          v_new_busn_trx_rec.parent_doc_split_num         := v_g17_parent_busn_trx_rec.document_split_num;

                          v_new_busn_trx_rec.unlinked_doc_flag            := 'N';
                          v_new_busn_trx_rec.historical_link_flag         := 'N';
                          v_new_busn_trx_rec.secondary_costing_flag       := v_g17_parent_busn_trx_rec.secondary_costing_flag;
                          v_new_busn_trx_rec.document_family_code         := v_g17_sales_busn_trx_rec.document_family_code;

                          v_new_busn_trx_rec.created_date                 := SYSDATE;
                          v_new_busn_trx_rec.created_by                   := v_g17_parent_busn_trx_rec.created_by;
                          v_new_busn_trx_rec.last_updated_date            := SYSDATE;
                          v_new_busn_trx_rec.last_updated_by              := v_g17_parent_busn_trx_rec.created_by;
                          v_new_busn_trx_rec.deleted_date                 := null;
                          v_new_busn_trx_rec.deleted_by                   := null;

                          --inserto
                          insert_busn_trx_row_sp(
                          p_insert_busn_trx_tab_row    => v_new_busn_trx_rec
                          );

                          --Determino que ya no encontré el G17
                          v_sale_node_found_flag   := FALSE;

                          --Pregunto si más abajo del registro insertado hay otro registro
                          --si es verdadero, verifico que el parente correspondía al registro anterior.
                          --si es así le cambio el parent con la información del registro insertado (88)
                          IF  v_cur_busn_trx%FOUND
                          AND v_busn_trx_tab_row.parent_doc_portfolio_type = v_g17_parent_busn_trx_rec.doc_portfolio_type
                          AND v_busn_trx_tab_row.parent_doc_comm_loc       = v_g17_parent_busn_trx_rec.doc_comm_location_code
                          AND v_busn_trx_tab_row.parent_doc_year           = v_g17_parent_busn_trx_rec.document_year
                          AND v_busn_trx_tab_row.parent_doc_month          = v_g17_parent_busn_trx_rec.document_month
                          AND v_busn_trx_tab_row.parent_doc_type_code      = v_g17_parent_busn_trx_rec.doc_type_code
                          AND v_busn_trx_tab_row.parent_doc_number         = v_g17_parent_busn_trx_rec.document_number
                          AND v_busn_trx_tab_row.parent_doc_item_num       = v_g17_parent_busn_trx_rec.document_item_num
                          AND v_busn_trx_tab_row.parent_doc_split_num      = v_g17_parent_busn_trx_rec.document_split_num
                          THEN   --cambio el parent con la información del registro insertado (88)
                                  UPDATE ODS_BUSINESS_TRANSACTIONS
                                  SET  PARENT_DOC_PORTFOLIO_TYPE = C_MILL_ACK_PORTFOLIO_TYPE
                                      ,PARENT_DOC_COMM_LOC       = V_G17_SALES_BUSN_TRX_REC.DOC_COMM_LOCATION_CODE
                                      ,PARENT_DOC_YEAR           = V_G17_SALES_BUSN_TRX_REC.DOCUMENT_YEAR
                                      ,PARENT_DOC_MONTH          = V_G17_SALES_BUSN_TRX_REC.DOCUMENT_MONTH
                                      ,PARENT_DOC_TYPE_CODE      = C_NULL_ACK_DOC_TYPE
                                      ,PARENT_DOC_NUMBER         = V_G17_SALES_BUSN_TRX_REC.DOCUMENT_NUMBER
                                      ,PARENT_DOC_ITEM_NUM       = V_G17_SALES_BUSN_TRX_REC.DOCUMENT_ITEM_NUM
                                      ,PARENT_DOC_SPLIT_NUM      = V_G17_SALES_BUSN_TRX_REC.DOCUMENT_SPLIT_NUM
                                  WHERE 1=1
                                  AND   BUSINESS_ID       = V_BUSN_TRX_TAB_ROW.BUSINESS_ID
                                  AND   BUSINESS_SEQUENCE = V_BUSN_TRX_TAB_ROW.BUSINESS_SEQUENCE + V_CORRIMIENTO;
                          END IF;
                END IF;
        END LOOP;
END generate_doc_g17_conf_order_sp;

----------------------------------------------------------------------------------------------

PROCEDURE generate_g17_conf_order_sp
IS
        CURSOR cur_g17_uncompleted_business
        IS
        /*SELECT  DISTINCT t1.business_id
        FROM    ods_business_transactions t1,
                ods_business_doc_tree_wrk wrk
        WHERE   wrk.business_id = t1.business_id
        AND     t1.business_id IN (SELECT DISTINCT t2.business_id
                                   FROM   ods_business_transactions t2
                                   WHERE  t2.doc_comm_location_code = 'G17')
        AND     NOT EXISTS (SELECT 1
                            FROM   ods_business_transactions t3
                            WHERE  t3.doc_comm_location_code = 'G17'
                            AND    t3.doc_portfolio_type = '88'
                            AND    t3.business_id = t1.business_id);*/
       SELECT
              DISTINCT T1.BUSINESS_ID
       FROM   ODS_BUSINESS_TRANSACTIONS T1
             ,ODS_BUSINESS_DOC_TREE_WRK WRK
       WHERE  1=1
       AND    T1.BUSINESS_ID            = WRK.BUSINESS_ID
       AND    T1.DOC_COMM_LOCATION_CODE = ODS_COMMON_PKG.C_METALMECANICA_COMM_LOC
       AND    NOT EXISTS (
                          SELECT 1
                          FROM   ODS_BUSINESS_TRANSACTIONS T2
                          WHERE  1=1
                          AND    T2.BUSINESS_ID                  = T1.BUSINESS_ID
                          AND    T2.BUSINESS_SEQUENCE           >= T1.BUSINESS_SEQUENCE
                          AND    NVL(T2.BUSINESS_REFERENCE_ID,0) = NVL(T1.BUSINESS_REFERENCE_ID,0)
                          AND    T2.DOC_COMM_LOCATION_CODE       = ODS_COMMON_PKG.C_METALMECANICA_COMM_LOC
                          AND    T2.DOC_PORTFOLIO_TYPE           = C_MILL_ACK_PORTFOLIO_TYPE
                         );

       	v_g17_business_id       cur_g17_uncompleted_business%rowtype;	--ods_business_transactions.BUSINESS_ID%type;

BEGIN
        FOR v_g17_business_id IN cur_g17_uncompleted_business
        LOOP
                -- Por cada Business en ods_business_doc_tree_wrk, con Origen G17 y que están incompletos,
                -- ejecutamos la generacion del nodo de Conforming Order.
                generate_doc_g17_conf_order_sp(
                        p_g17_business_id => v_g17_business_id.business_id
                        );
        END LOOP;
END generate_g17_conf_order_sp;


/*--Se comentan los SP update_spij_business_sp-->update_doc_spij_business_sp ya que son remplazados por el update_complex_branches_bus_sp
PROCEDURE update_doc_spij_business_sp(
        p_spi_business_id    IN ods_business_transactions.business_id%TYPE
        )
IS
        --
        v_cur_busn_trx         		ods_common_pkg.t_refcursor;
        v_busn_trx_tab_row    		t_busn_trx_tab_row;
        --
        v_process_date          	DATE:= SYSDATE;
        --
BEGIN
        -- recuperamos la cadena
        get_business_transaction_sp(
                p_business_id        	=> p_spi_business_id,
                p_include_manual_flag   => ods_common_pkg.c_str_false,
                p_cur_busn_trx        	=> v_cur_busn_trx
                );

        -- verificamos si se encontro la cadena
        FETCH v_cur_busn_trx INTO v_busn_trx_tab_row;

        WHILE v_cur_busn_trx%FOUND
        LOOP
                --Se vuelve atras el cambio que cambiaba el portfolio de 06 a 04. Se deja el portfolio que viene de SAP, es decir 06.
                --
                ---- Si se encontró la cadena, se busca el primer nodo Commercial Location = SPI o TSA o KPT y que sea portfolio 06
                --IF   v_busn_trx_tab_row.doc_comm_location_code IN (c_spij_comm_loc,c_tsa_comm_loc,c_kpt_comm_loc)       --SPI,TSA,KPT
                -- AND v_busn_trx_tab_row.doc_portfolio_type = c_prod_operat_portfolio_type                               --06
                --THEN
                --        --Actualizo el primer nodo encontrado con portfolio 04 de postproceso
                --        UPDATE  ods_business_transactions trx
                --        SET     trx.doc_portfolio_type = c_post_process_portfolio_type --04
                --        WHERE   trx.business_id = p_spi_business_id
                --        AND     trx.business_sequence = v_busn_trx_tab_row.business_sequence;
                --        --Actualizo el nodo siguiente para modificarlo como padre
                --        UPDATE  ods_business_transactions trx
                --        SET     trx.parent_doc_portfolio_type = c_post_process_portfolio_type --04
                --        WHERE   trx.business_id = p_spi_business_id
                --        AND     trx.business_sequence = v_busn_trx_tab_row.business_sequence + 1;
                --
                --END IF;
                --
                --Si se encontro la cadena, se busca Commercial Location = SPI o TSA o KPT y que se conferma (88) para poner el flag de secondary en Y
                IF   v_busn_trx_tab_row.doc_comm_location_code IN (c_spij_comm_loc,c_tsa_comm_loc,c_kpt_comm_loc)       --SPI,TSA,KPT
                 AND v_busn_trx_tab_row.doc_portfolio_type = c_mill_ack_portfolio_type                                  --88
                THEN
                        --Actualiza en Y el Secondary Costing
                        UPDATE  ods_business_transactions trx
                        SET     trx.secondary_costing_flag = ods_common_pkg.c_str_yes
                        WHERE   trx.business_id = p_spi_business_id
                        AND     trx.business_sequence = v_busn_trx_tab_row.business_sequence;
                        --
                END IF;
                --
                FETCH v_cur_busn_trx INTO v_busn_trx_tab_row;
                --
        END LOOP;
        --
END update_doc_spij_business_sp;*/


/*--Se comentan los SP update_spij_business_sp-->update_doc_spij_business_sp ya que son remplazados por el update_complex_branches_bus_sp
PROCEDURE update_spij_business_sp
IS
        --
        CURSOR cur_spi_upd_business
        IS
        SELECT  DISTINCT t1.business_id
        FROM    ods_business_transactions t1
        WHERE   0=0
        -- Se agregan 2 nuevas sociedades al cursor de procesamiento, TSA y KPT.
        AND     t1.doc_comm_location_code IN (c_spij_comm_loc,c_tsa_comm_loc,c_kpt_comm_loc)
        AND     t1.doc_portfolio_type = c_prod_operat_portfolio_type;
        --
        v_spi_upd_business_id   cur_spi_upd_business%ROWTYPE;
        --
BEGIN
        --
        FOR v_spi_upd_business_id IN cur_spi_upd_business
        LOOP
                -- Por cada Business en ods_business_transactions, con Origen SPI y DOC_PORTFOLIO_TYPE = '06'
                -- actualizamos el DOC_PORTFOLIO_TYPE con el valor de '04'.
                --Se agregan dos origenes mas, TSA y KPT
                update_doc_spij_business_sp(
                        p_spi_business_id => v_spi_upd_business_id.business_id
                        );
        END LOOP;
        --
END update_spij_business_sp;*/

PROCEDURE generate_doc_link_extornos_sp(
        --
        p_ext_parent_doc_comm_loc_code      IN ods_business_transactions.parent_doc_comm_loc%TYPE,
        p_ext_parent_doc_year               IN ods_business_transactions.parent_doc_year%TYPE,
        p_ext_parent_doc_month              IN ods_business_transactions.parent_doc_month%TYPE,
        p_ext_parent_doc_number             IN ods_business_transactions.parent_doc_number%TYPE,
        p_ext_parent_doc_item_num           IN ods_business_transactions.parent_doc_item_num%TYPE,
        p_ext_parent_doc_split_num          IN ods_business_transactions.parent_doc_split_num%TYPE,
        --
        p_ext_child_doc_comm_loc_code       IN ods_business_transactions.doc_comm_location_code%TYPE,
        p_ext_child_doc_year                IN ods_business_transactions.document_year%TYPE,
        p_ext_child_doc_month               IN ods_business_transactions.document_month%TYPE,
        p_ext_child_doc_number              IN ods_business_transactions.document_number%TYPE,
        p_ext_child_doc_item_num            IN ods_business_transactions.document_item_num%TYPE,
        p_ext_child_doc_split_num           IN ods_business_transactions.document_split_num%TYPE,
        --
        p_extorno_doc_type_code             IN ods_business_transactions.doc_type_code%TYPE
        )
IS
        --
        v_parent_bid                    ods_business_transactions.business_id%TYPE;
        v_parent_seq                    ods_business_transactions.business_sequence%TYPE;
        v_parent_max_seq                ods_business_transactions.business_sequence%TYPE;
        --
        v_child_bid                     ods_business_transactions.business_id%TYPE;
        v_child_seq                     ods_business_transactions.business_sequence%TYPE;
        --
        v_new_busn_trx_rec              t_busn_trx_tab_row;
        v_busn_trx_rec                  t_busn_trx_tab_row;
        v_busn_trx_cur                  t_refcursor;
        --
        v_busn_trx_data_table           ods_busn_trx_table_type;
        --
        v_process_date                  DATE:= SYSDATE;
        v_nodo_number                   PLS_INTEGER := 0;
        v_dummy                         PLS_INTEGER;
        --
BEGIN
        --
--        dbms_output.PUT_LINE('generate_doc_link_extornos_sp - Inicio');
--        dbms_output.PUT_LINE('--------------------------------------');
--        dbms_output.PUT_LINE('p_parent_doc_comm_loc_code: '||p_parent_doc_comm_loc_code);
--        dbms_output.PUT_LINE('p_parent_doc_year: '||p_parent_doc_year);
--        dbms_output.PUT_LINE('p_parent_doc_month: '||p_parent_doc_month);
--        dbms_output.PUT_LINE('p_doc_type_code: '||p_doc_type_code);
--        dbms_output.PUT_LINE('p_doc_portfolio_type: '||p_doc_portfolio_type);
--        dbms_output.PUT_LINE('p_parent_doc_number: '||p_parent_doc_number);
--        dbms_output.PUT_LINE('p_parent_doc_item_num: '||p_parent_doc_item_num);
--        dbms_output.PUT_LINE('p_parent_doc_split_num: '||p_parent_doc_split_num);
--        dbms_output.PUT_LINE('p_child_doc_comm_loc_code: '||p_child_doc_comm_loc_code);
--        dbms_output.PUT_LINE('p_child_doc_year: '||p_child_doc_year);
--        dbms_output.PUT_LINE('p_child_doc_month: '||p_child_doc_month);
--        dbms_output.PUT_LINE('p_child_doc_number: '||p_child_doc_number);
--        dbms_output.PUT_LINE('p_child_doc_item_num: '||p_child_doc_item_num);
--        dbms_output.PUT_LINE('p_child_doc_split_num: '||p_child_doc_split_num);
--        dbms_output.PUT_LINE('--------------------------------------');
        --
        --Buscamos el Business_Id al que pertenece la orden del Parent correspondiente al Extorno pero con un DOC_TYPE_CODE =' 00'
	get_busn_id_sp(
                p_doc_comm_location_code        => p_ext_parent_doc_comm_loc_code,
                p_document_year                 => p_ext_parent_doc_year,
                p_document_month                => p_ext_parent_doc_month,
                p_doc_type_code                 => c_null_ack_doc_type,         --Constante con valor DOC_TYPE_CODE =' 00'
                p_document_number               => p_ext_parent_doc_number,
                p_document_item_num             => p_ext_parent_doc_item_num,
                p_document_split_num            => p_ext_parent_doc_split_num ,
                p_historical_flag        	=> ods_common_pkg.c_str_false,
                p_business_id                   => v_parent_bid,
                p_business_sequence             => v_parent_seq
                );

        --Buscamos la secuencia máxima para el Parent
        SELECT  MAX(bt.business_sequence)
        INTO    v_parent_max_seq
        FROM    ods_business_transactions bt
        WHERE   bt.business_id = v_parent_bid;
        --
        --Buscamos el Business al que pertenece la orden del Child correspondiente al Extorno pero con un DOC_TYPE_CODE =' 00'
        get_busn_id_sp(
                p_doc_comm_location_code        => p_ext_child_doc_comm_loc_code,
                p_document_year                 => p_ext_child_doc_year,
                p_document_month                => p_ext_child_doc_month,
                p_doc_type_code                 => c_null_ack_doc_type,         --Constante con valor DOC_TYPE_CODE =' 00'
                p_document_number               => p_ext_child_doc_number,
                p_document_item_num             => p_ext_child_doc_item_num,
                p_document_split_num            => p_ext_child_doc_split_num,
                p_historical_flag               => ods_common_pkg.c_str_false,
                p_business_id                   => v_child_bid,
                p_business_sequence             => v_child_seq
                );
        --
        IF v_parent_bid IS NOT NULL AND v_child_bid IS NOT NULL
        THEN

                --Cargamos los registros de ODS_BUSINESS_TRANSACTIONS en una colección para luego poder consultarla.
                retrieve_busn_trx_data_sp (
                        p_business_id           => v_child_bid,
                        p_busn_trx_data_table   => v_busn_trx_data_table
                        );

                --Recorremos la cadena a la cual pertenece el documento Child, desde ese Business Sequence en adelante,
                --insertando dichos documentos al final de la cadena a la cual pertenece el documento Parent
                OPEN v_busn_trx_cur FOR SELECT  business_id,
                                                business_sequence,
                                                business_reference_id,
                                                business_reference_seq,
                                                doc_portfolio_type,
                                                doc_comm_location_code,
                                                document_year,
                                                document_month,
                                                doc_type_code,
                                                document_number,
                                                document_item_num,
                                                document_split_num,
                                                unlinked_doc_flag,
                                                data_source_system,
                                                parent_doc_comm_loc,
                                                parent_doc_portfolio_type,
                                                parent_doc_year,
                                                parent_doc_month,
                                                parent_doc_type_code,
                                                parent_doc_number,
                                                parent_doc_item_num,
                                                parent_doc_split_num,
                                                historical_link_flag,
                                                secondary_costing_flag,
                                                origin_business_id,
                                                document_family_code,
                                                manual_record_flag,
                                                integration_type,
                                                created_date,
                                                created_by,
                                                last_updated_date,
                                                last_updated_by,
                                                deleted_date,
                                                deleted_by
                                        FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type))
                                        WHERE	business_sequence >= v_child_seq
                                        ORDER BY business_sequence;
                LOOP
                FETCH v_busn_trx_cur INTO v_busn_trx_rec;       -- fetch from cursor variable
                EXIT WHEN v_busn_trx_cur%NOTFOUND; 		-- exit when last row is fetched
                        --
                        v_parent_max_seq := v_parent_max_seq + 1;
                        v_nodo_number := v_nodo_number + 1;
                        --
                        -- armamos el nodo nuevo
                        v_new_busn_trx_rec.business_id                  := v_parent_bid;
                        v_new_busn_trx_rec.business_sequence            := v_parent_max_seq;
                        v_new_busn_trx_rec.business_reference_id        := v_child_bid;
                        v_new_busn_trx_rec.business_reference_seq       := v_busn_trx_rec.business_sequence;  		--v_business_reference_seq;	--1;
                        v_new_busn_trx_rec.doc_portfolio_type           := v_busn_trx_rec.doc_portfolio_type;		--c_mill_ack_portfolio_type;
                        v_new_busn_trx_rec.doc_comm_location_code       := v_busn_trx_rec.doc_comm_location_code; 	--p_child_doc_comm_loc_code;
                        v_new_busn_trx_rec.document_year                := v_busn_trx_rec.document_year;  		--p_child_doc_year;
                        v_new_busn_trx_rec.document_month               := v_busn_trx_rec.document_month;		--p_child_doc_month;
                        v_new_busn_trx_rec.doc_type_code                := v_busn_trx_rec.doc_type_code;		--c_null_ack_doc_type;
                        v_new_busn_trx_rec.document_number              := v_busn_trx_rec.document_number;		--p_child_doc_number;
                        v_new_busn_trx_rec.document_item_num            := v_busn_trx_rec.document_item_num;		--p_child_doc_item_num;
                        v_new_busn_trx_rec.document_split_num           := v_busn_trx_rec.document_split_num;		--p_child_doc_split_num;
                        v_new_busn_trx_rec.data_source_system           := v_busn_trx_rec.data_source_system;		--v_parent_data_source_system;
                        v_new_busn_trx_rec.parent_doc_comm_loc          := CASE
                                                                                WHEN v_nodo_number = 1 THEN p_ext_parent_doc_comm_loc_code
                                                                                ELSE v_busn_trx_rec.parent_doc_comm_loc
                                                                           END;
                        v_new_busn_trx_rec.parent_doc_portfolio_type    := CASE /*Asigno DOC_PORTFOLIIO_TYPE ='88' si es el primer nodo Parent, caso contrario
                                                                                  el que obtengo de la BT*/
                                                                                WHEN v_nodo_number = 1 THEN c_mill_ack_portfolio_type --DOC_PORTFOLIO_TYPE = '88'
                                                                                ELSE v_busn_trx_rec.parent_doc_portfolio_type
                                                                           END;
                        v_new_busn_trx_rec.parent_doc_year              := CASE
                                                                                WHEN v_nodo_number = 1 THEN p_ext_parent_doc_year
                                                                                ELSE v_busn_trx_rec.parent_doc_year
                                                                           END;
                        v_new_busn_trx_rec.parent_doc_month             := CASE
                                                                                WHEN v_nodo_number = 1 THEN p_ext_parent_doc_month
                                                                                ELSE v_busn_trx_rec.parent_doc_month
                                                                           END;
                        v_new_busn_trx_rec.parent_doc_type_code         := CASE /*Asigno DOC_TYPE_CODE ='00' si es el primer nodo Parent, caso contrario el que
                                                                                  obtengo de la BT*/
                                                                                WHEN v_nodo_number = 1 THEN c_null_ack_doc_type       --DOC_TYPE_CODE ='00'
                                                                                ELSE v_busn_trx_rec.parent_doc_type_code
                                                                           END;
                        v_new_busn_trx_rec.parent_doc_number            := CASE
                                                                                WHEN v_nodo_number = 1 THEN p_ext_parent_doc_number
                                                                                ELSE v_busn_trx_rec.parent_doc_number
                                                                           END;
                        v_new_busn_trx_rec.parent_doc_item_num          := CASE
                                                                                WHEN v_nodo_number = 1 THEN p_ext_parent_doc_item_num
                                                                                ELSE v_busn_trx_rec.parent_doc_item_num
                                                                           END;
                        v_new_busn_trx_rec.parent_doc_split_num         := CASE
                                                                                WHEN v_nodo_number = 1 THEN p_ext_parent_doc_split_num
                                                                                ELSE v_busn_trx_rec.parent_doc_split_num
                                                                           END;
                        v_new_busn_trx_rec.unlinked_doc_flag        := ods_common_pkg.c_str_no;
                        v_new_busn_trx_rec.historical_link_flag     := ods_common_pkg.c_str_no;
                        v_new_busn_trx_rec.secondary_costing_flag   := v_busn_trx_rec.secondary_costing_flag;
                        v_new_busn_trx_rec.origin_business_id       := v_busn_trx_rec.origin_business_id;
                        v_new_busn_trx_rec.document_family_code     := v_busn_trx_rec.document_family_code;
                        v_new_busn_trx_rec.integration_type         := v_busn_trx_rec.integration_type;
                        v_new_busn_trx_rec.created_date             := v_process_date;
                        v_new_busn_trx_rec.created_by               := 'Extornos';
                        v_new_busn_trx_rec.last_updated_date        := v_process_date;
                        v_new_busn_trx_rec.last_updated_by          := 'Extornos';
                        v_new_busn_trx_rec.deleted_date             := NULL;
                        v_new_busn_trx_rec.deleted_by               := NULL;
                        --
                        -- Damos de alta el nuevo nodo en ods_business_transactions correspondiente a la cadena informada como Parent en ods_busn_transaction_extornos.
                        insert_busn_trx_row_sp(
                                p_insert_busn_trx_tab_row => v_new_busn_trx_rec
                                );
                        --
                END LOOP;
                --
                CLOSE v_busn_trx_cur;
                --
                --Actualizamos la tabla ods_busn_transaction_extornos con el Business_Id correspondiente a la cadena que contiene el doc informado como Parent
                UPDATE  ods_busn_transaction_extornos ext
                SET	ext.business_id = v_parent_bid,
                        ext.last_updated_date = v_process_date,
                        ext.last_updated_by = 'Extornos'
                WHERE  	ext.parent_doc_comm_loc_code = p_ext_parent_doc_comm_loc_code
                AND	ext.parent_doc_year = p_ext_parent_doc_year
                AND	ext.parent_doc_month = p_ext_parent_doc_month
                AND 	ext.parent_doc_num = p_ext_parent_doc_number
                AND	ext.parent_doc_item_num = p_ext_parent_doc_item_num
                AND 	ext.parent_doc_split_num = p_ext_parent_doc_split_num;
                --
                --En la ODS_BUSINESS_TRANSACTIONS actualizamos el campo SECONDARY_COSTING_FLAG en 'Y' solo para aquellos casos en los que el DOC_TYPE_CODE
                --informado en la tabla ODS_BUSN_TRANSACTION_EXTORNOS sea 'IM' (Importado).
                IF p_extorno_doc_type_code = 'IM'
                THEN
                        --
                        UPDATE  ods_business_transactions bt
                        SET	bt.secondary_costing_flag = ods_common_pkg.c_str_yes
                        WHERE	bt.business_id = v_parent_bid
                        AND	bt.business_sequence = v_parent_seq;
                        --
                END IF;
                --
                BEGIN
                        --
                        SELECT  1
                        INTO    v_dummy
                        FROM    ods_business_doc_tree_wrk wrk
                        WHERE   wrk.business_id = v_parent_bid;
                        --
                EXCEPTION
                	WHEN no_data_found THEN
                    	        INSERT INTO ods_business_doc_tree_wrk (
                                        business_id,
                                        process_status,
                                        created_date,
                                        created_by,
                                        last_updated_date,
                                        last_updated_by)
				        VALUES (
                                                v_parent_bid,
                            	                'A',
                                                SYSDATE,
                                                'ONE',
                                                SYSDATE,
                                                'ONE'
                                                );
		END;
                --
        END IF;
        --
END generate_doc_link_extornos_sp;


PROCEDURE generate_bt_link_extornos_sp
IS
        --
        CURSOR cur_link_extornos
        IS
        SELECT  ext.parent_doc_comm_loc_code,
                ext.parent_doc_year,
                ext.parent_doc_month,
                ext.parent_doc_num,
                ext.parent_doc_item_num,
                ext.parent_doc_split_num,
                ext.doc_comm_loc_code,
                ext.doc_year,
                ext.doc_month,
                ext.doc_num,
                ext.doc_item_num,
                ext.doc_split_num,
                --
                ext.doc_type_code
                --
        FROM    ods_busn_transaction_extornos ext
        WHERE   ext.business_id = 999999;
        --

        v_link_extornos_row     cur_link_extornos%ROWTYPE;

BEGIN
        --
        --Recorremos los registros obtenidos en el cursor de Extornos
        FOR v_link_extornos_row IN cur_link_extornos
        LOOP
                --
                --Invocamos al SP que se encarga de encontrar la cadena asociada al documento informado como Parent en el cursor de Extornos y luego la
                --completa con la informacion del documento informado como Child en el cursor de Extornos.
                --Cargamos el parametro de DOC_TYPE_CODE con el valor que tiene la tabla ODS_BUSN_TRANSACTION_EXTORNOS
                generate_doc_link_extornos_sp (
                                --
                                p_ext_parent_doc_comm_loc_code  => v_link_extornos_row.parent_doc_comm_loc_code,
                                p_ext_parent_doc_year           => v_link_extornos_row.parent_doc_year,
                                p_ext_parent_doc_month          => v_link_extornos_row.parent_doc_month,
                                p_ext_parent_doc_number         => v_link_extornos_row.parent_doc_num,
                                p_ext_parent_doc_item_num       => v_link_extornos_row.parent_doc_item_num,
                                p_ext_parent_doc_split_num      => v_link_extornos_row.parent_doc_split_num,
                                --
                                p_ext_child_doc_comm_loc_code   => v_link_extornos_row.doc_comm_loc_code,
                                p_ext_child_doc_year            => v_link_extornos_row.doc_year,
                                p_ext_child_doc_month           => v_link_extornos_row.doc_month,
                                p_ext_child_doc_number          => v_link_extornos_row.doc_num,
                                p_ext_child_doc_item_num        => v_link_extornos_row.doc_item_num,
                                p_ext_child_doc_split_num       => v_link_extornos_row.doc_split_num,
                                --
                                p_extorno_doc_type_code         => v_link_extornos_row.doc_type_code
                                );
        END LOOP;
        --
END generate_bt_link_extornos_sp;

----------------------------------------------------------------------------------------------
PROCEDURE retrieve_busn_trx_data_sp (
        p_business_id           IN ods_business_transactions.business_id%TYPE,
        p_busn_trx_data_table   OUT ods_busn_trx_table_type)
IS
        --
        CURSOR cur_busn_trx_data (p_c_business_id IN ods_business_transactions.business_id%TYPE)
        IS
        SELECT  business_id,
                business_sequence,
                business_reference_id,
                business_reference_seq,
                doc_portfolio_type,
                doc_comm_location_code,
                document_year,
                document_month,
                doc_type_code,
                document_number,
                document_item_num,
                document_split_num,
                unlinked_doc_flag,
                data_source_system,
                parent_doc_comm_loc,
                parent_doc_portfolio_type,
                parent_doc_year,
                parent_doc_month,
                parent_doc_type_code,
                parent_doc_number,
                parent_doc_item_num,
                parent_doc_split_num,
                historical_link_flag,
                secondary_costing_flag,
                origin_business_id,
                document_family_code,
                'N' manual_record_flag,
                integration_type,
                created_date,
                created_by,
                last_updated_date,
                last_updated_by,
                deleted_date,
                deleted_by
        FROM    ods_business_transactions
        WHERE   business_id = p_c_business_id
        ORDER BY business_sequence;
        --

        v_busn_trx_data_rec     cur_busn_trx_data%ROWTYPE;
        v_busn_trx_data_table   ods_busn_trx_table_type := ods_busn_trx_table_type ();
        --
BEGIN
        -- cargamos la object collection para poder consultarla luego como una tabla
        FOR v_busn_trx_data_rec IN cur_busn_trx_data (p_c_business_id => p_business_id)
        LOOP
                -- generamos un registro para poder ejecutar la query sin registros
                v_busn_trx_data_table.EXTEND;
                v_busn_trx_data_table (v_busn_trx_data_table.COUNT) :=
                        ods_busn_trx_row_type (
                                v_busn_trx_data_rec.business_id,
                                v_busn_trx_data_rec.business_sequence,
                                v_busn_trx_data_rec.business_reference_id,
                                v_busn_trx_data_rec.business_reference_seq,
                                v_busn_trx_data_rec.doc_portfolio_type,
                                v_busn_trx_data_rec.doc_comm_location_code,
                                v_busn_trx_data_rec.document_year,
                                v_busn_trx_data_rec.document_month,
                                v_busn_trx_data_rec.doc_type_code,
                                v_busn_trx_data_rec.document_number,
                                v_busn_trx_data_rec.document_item_num,
                                v_busn_trx_data_rec.document_split_num,
                                v_busn_trx_data_rec.unlinked_doc_flag,
                                v_busn_trx_data_rec.data_source_system,
                                v_busn_trx_data_rec.parent_doc_comm_loc,
                                v_busn_trx_data_rec.parent_doc_portfolio_type,
                                v_busn_trx_data_rec.parent_doc_year,
                                v_busn_trx_data_rec.parent_doc_month,
                                v_busn_trx_data_rec.parent_doc_type_code,
                                v_busn_trx_data_rec.parent_doc_number,
                                v_busn_trx_data_rec.parent_doc_item_num,
                                v_busn_trx_data_rec.parent_doc_split_num,
                                v_busn_trx_data_rec.historical_link_flag,
                                v_busn_trx_data_rec.secondary_costing_flag,
                                v_busn_trx_data_rec.origin_business_id,
                                v_busn_trx_data_rec.document_family_code,
                                v_busn_trx_data_rec.manual_record_flag,
                                v_busn_trx_data_rec.integration_type,
                                v_busn_trx_data_rec.created_date,
                                v_busn_trx_data_rec.created_by,
                                v_busn_trx_data_rec.last_updated_date,
                                v_busn_trx_data_rec.last_updated_by,
                                v_busn_trx_data_rec.deleted_date,
                                v_busn_trx_data_rec.deleted_by);
        END LOOP;
        --
        p_busn_trx_data_table := v_busn_trx_data_table;
        --
END retrieve_busn_trx_data_sp;

--------------------------------------------------------------------------------
FUNCTION get_busn_trx_created_date_fn (p_busn_trx_data_table	IN	ods_busn_trx_table_type
                    )
RETURN ods_busn_tree_classifications.created_date%TYPE
IS
    v_result    ods_business_transactions.CREATED_DATE%TYPE;
    v_business_sequence		ods_business_transactions.BUSINESS_SEQUENCE%TYPE;
    v_doc_portfolio_type     ods_business_transactions.DOC_PORTFOLIO_TYPE%TYPE;
    v_test 	ods_business_transactions.BUSINESS_ID%TYPE;
BEGIN
    BEGIN
	        --Recuperamos el Doc_Portfolio_Type correspondiente al menor Business_Sequence
            --en donde el Portfolio_Type sea Sales, Replenishment o Purchase
            select	doc_portfolio_type, business_sequence
            into	v_doc_portfolio_type,	v_business_sequence
			from
 				(SELECT	business_id            ,
				        business_sequence              ,
            			doc_portfolio_type ,
            			min(business_sequence) over (partition by business_id) min_sequence
    			 FROM   TABLE(CAST(p_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table
                 WHERE	doc_portfolio_type in ('02','90','01')
               )
			where business_sequence = min_sequence;


    EXCEPTION
            WHEN NO_DATA_FOUND THEN
                    v_result := NULL;
	END;



    IF v_doc_portfolio_type IS NOT NULL
    THEN

        IF v_doc_portfolio_type = '02'
        THEN
                BEGIN
                        --select s.item_date
                        select s.LEGACY_CREATED_DATE
                        into v_result
                        from ods_sale_orders_itm s,
                             TABLE(CAST(p_busn_trx_data_table AS ods_busn_trx_table_type)) t
                        where s.commercial_location_code = t.doc_comm_location_code
                        and s.doc_type_code            = t.doc_type_code
                        and s.so_order_year            = t.document_year
                        and s.so_order_month           = t.document_month
                        and s.so_number                = t.document_number
                        and s.so_item_num              = t.document_item_num
                        and t.DOC_PORTFOLIO_TYPE = '02'
                        and t.BUSINESS_SEQUENCE = v_business_sequence;
                EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            v_result := NULL;

                END;


        ELSIF v_doc_portfolio_type = '90'
        THEN
                BEGIN
                        --Select r.item_date
                        Select r.LEGACY_CREATED_DATE
                        into v_result
                        from adas.ods_replenishment_orders_itm r,
                            TABLE(CAST(p_busn_trx_data_table AS ods_busn_trx_table_type)) t
                        where r.commercial_location_code = t.doc_comm_location_code
                        and r.ro_doc_type_code         = t.doc_type_code
                        and r.ro_order_year            = t.document_year
                        and r.ro_order_month           = t.document_month
                        and r.ro_number                = t.document_number
                        and r.ro_item_num              = t.document_item_num
                        and t.DOC_PORTFOLIO_TYPE = '90'
                        and t.BUSINESS_SEQUENCE = v_business_sequence;

                EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            v_result := NULL;

                END;
        ELSIF v_doc_portfolio_type = '01'
        THEN
                BEGIN
                        select p.item_date
                        into v_result
                        from adas.ods_purchase_orders_itm p,
                        TABLE(CAST(p_busn_trx_data_table AS ods_busn_trx_table_type)) t
                        where p.po_number      = t.document_number
                        and p.po_item_num    = t.document_item_num
                        and p.commercial_location_code = t.doc_comm_location_code
                        and p.doc_type_code  = t.doc_type_code
                        and p.po_order_year  = t.document_year
                        and p.po_order_month = t.document_month
                        and t.DOC_PORTFOLIO_TYPE = '01'
                        and t.BUSINESS_SEQUENCE = v_business_sequence;
                EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            v_result := NULL;

                END;
        END IF;

	END IF;
        --
        RETURN v_result;
        --
END get_busn_trx_created_date_fn;

--------------------------------------------------------------------------------

--////////////////// PUBLIC MODULES IMPLEMENTATION ////////////////////////

--------------------------------------------------------------------------------
FUNCTION is_build_to_order_busn_str_fn(
        p_business_id     IN ods_business_transactions.business_id%TYPE
        )
RETURN VARCHAR2
IS
        v_result ods_common_pkg.s_str_boolean := ods_common_pkg.c_str_false;
BEGIN
        IF ods_business_doc_tree_pkg.is_build_to_order_doc_tree_fn(p_business_id)
        THEN
                v_result:= ods_common_pkg.c_str_true;
        END IF;

        RETURN v_result;
END is_build_to_order_busn_str_fn;

--------------------------------------------------------------------------------

FUNCTION is_busn_trx_ref_no_found_fn(
        p_message_desc     IN ods_busn_doc_tree_error_pkg.s_error_desc
        )
RETURN BOOLEAN
IS
BEGIN
        RETURN (c_busn_trx_ref_no_found_desc = p_message_desc);

END is_busn_trx_ref_no_found_fn;

--------------------------------------------------------------------------------

FUNCTION is_build_stock_doc_tree_str_fn(
        p_business_id    ods_business_doc_tree_wrk.business_id%TYPE
        )
RETURN VARCHAR2
IS
        v_root_busn_doc_tree_id    ods_business_document_tree.business_document_tree_id%TYPE;
    v_result    ods_common_pkg.s_str_boolean := ods_common_pkg.c_str_false;
BEGIN
        -- obtenemos el identificador del nodo raiz del arbol de doc
        v_root_busn_doc_tree_id:= ods_business_doc_tree_pkg.find_doc_tree_root_doc_id_fn(
                p_business_id => p_business_id
                );

    -- verificamos si se trata de un arbol puro Stock
    IF ods_business_doc_tree_pkg.is_build_to_stock_doc_tree_fn(
        p_business_document_tree_id => v_root_busn_doc_tree_id
                )
    THEN
        v_result := ods_common_pkg.c_str_true;
    END IF;

        RETURN v_result;
END is_build_stock_doc_tree_str_fn;

--------------------------------------------------------------------------------
PROCEDURE generate_busn_trx_sp
IS
        CURSOR cur_busn_doc_tree_wrk
        IS
        SELECT  business_id
        FROM    (
                -- business id que son referenciados por otros
                SELECT  a.business_id,
                        DECODE (ods_business_transaction_pkg.is_build_stock_doc_tree_str_fn(business_id),ods_common_pkg.c_str_true, 5,
                                                                                                        3) process_sequence
                FROM    (
                        SELECT  business_id
                        FROM    ods_business_doc_tree_wrk
                        UNION
                        SELECT  business_id
                        FROM    ods_business_doc_tree_wrk_err
                        WHERE   reprocess_qty < 3
                        ) a
                WHERE   EXISTS (
                                SELECT  1
                                FROM    ods_business_transactions b
                                WHERE   b.business_reference_id = a.business_id )
                                UNION ALL
                                -- business id que no son referenciados por ninguno
                                SELECT  a.business_id,
                                        DECODE(ods_business_transaction_pkg.is_build_to_order_busn_str_fn(a.business_id),ods_common_pkg.c_str_true,1,
                                                DECODE (ods_business_transaction_pkg.is_build_stock_doc_tree_str_fn(business_id),ods_common_pkg.c_str_true, 5,
                                                                                                                                2)
                                              ) process_sequence
                                FROM    (
                                        SELECT  business_id
                                        FROM    ods_business_doc_tree_wrk
                                        UNION
                                        SELECT  business_id
                                        FROM    ods_business_doc_tree_wrk_err
                                        WHERE   reprocess_qty < 3
                                        ) a
                                WHERE   NOT EXISTS (
                                                    SELECT  1
                                                    FROM    ods_business_transactions b
                                                    WHERE   b.business_reference_id = a.business_id )
                )
                ORDER BY
                        process_sequence,
                        business_id;

        v_busn_doc_tree_wrk_rec cur_busn_doc_tree_wrk%ROWTYPE;

        v_busn_trx_bkp_tab      t_busn_trx_bkp_tab;

        v_errm                  VARCHAR2(250);
	v_process               VARCHAR2(150);

BEGIN
        --
	v_process := 'get_busn_trx_ref_doc_tree_sp';

        -- generamos y recuperamos los BId a procesar para cadenas donde cambio la referencia
        -- backupeamos y eliminamos las cadenas que deben ser luego actualizadas
        get_busn_trx_ref_doc_tree_sp(
                p_busn_trx_bkp        => v_busn_trx_bkp_tab
                );

	v_process := 'process_build_to_order_trx_sp';

        -- procesamos las transacciones Build to Stock y Mixtas
        -- recorremos el cursor y procesamos por cada BId
        FOR v_busn_doc_tree_wrk_rec IN cur_busn_doc_tree_wrk
        LOOP
                -- se procesan los dos casos por igual
                process_build_to_order_trx_sp(
                        p_business_id    => v_busn_doc_tree_wrk_rec.business_id
                        );
        END LOOP;


	v_process := 'proc_changed_ref_busn_trx_sp';
        -- Procesamos las cadenas donde cambiaron las referenciadas
        proc_changed_ref_busn_trx_sp(
                p_busn_trx_bkp_tab    => v_busn_trx_bkp_tab
                );

	v_process := 'delete_error_busn_id_sp';

	-- borramos los business_id que tuvieron error
        delete_error_busn_id_sp;

	v_process := 'ods_busn_doc_tree_error_pkg.record_error_sp';
        -- almacenamos los business id con errores en la tabla
        ods_busn_doc_tree_error_pkg.record_error_sp(
                p_send_notification_flag        => TRUE,
                p_notification_address          => c_proc_responsible_email
                );

	v_process := 'mark_main_costing_branch_sp';
	-- invocamos el proceso de marcado de la rama mas significativa
	mark_main_costing_branch_sp;

	v_process := 'generate_bt_serv_sale_sp';
	-- ejecutamos el proceso de agregado de nodos service sale
        generate_bt_serv_sale_sp;

v_process := 'generate_g17_conf_order_sp'; -- se modifico en el ITTEN00367516
	-- completamos las cadenas con origen 'G17 - METALMECANICA'
      generate_g17_conf_order_sp;

        --Agregamos commit para poder consultar la ODS_BUSINESS_TRANSACTIONS
        --y agregar la información de Extornos
        COMMIT;

/*
 ----Se comentan los SP update_spij_business_sp-->update_doc_spij_business_sp ya que son remplazados por el update_complex_branches_bus_sp
	v_process := 'update_spij_business_sp';
        -- Actualizamos las cadenas con origen en SPIJ-TSA-KPT
        update_spij_business_sp;
--Se coloca la nueva funcion update_complex_branches_bus_sp luego de la generacion de los links de extornos
 */

	v_process := 'generate_bt_link_extornos_sp';
	--Completamos las cadenas con procesos de Extornos
        generate_bt_link_extornos_sp;

  -- agregamos commit dado que se trata de un proceso que se ejecuta solo desde
        -- script control M
        COMMIT;
--
  	v_process := 'update_complex_branches_bus_sp';
        -- marcar la SECONDARY_COSTING_FLAG si las branch's son complejas y son docuemntos de produccion (88)
        update_complex_branches_bus_sp;
--


EXCEPTION
        WHEN OTHERS THEN
                v_errm := SUBSTR(SQLERRM, 1 , 250);
                ods_error_log_pkg.insert_error_log_table (
                        p_process               => 'generate_busn_trx_sp'||' - '||v_process,
                        p_description           => 'EXCEPTION: others ' || SQLERRM,
                        p_description2          => NULL,
                        p_created_by            => 'TENARIS',
                        p_last_updated_date     => SYSDATE,
                        p_last_updated_by       => 'TENARIS');

END generate_busn_trx_sp;
--------------------------------------------------------------------------------

PROCEDURE get_business_transaction_sp(
        p_business_id        IN    ods_business_transactions.business_id%TYPE,
    p_include_manual_flag    IN     ods_common_pkg.s_str_boolean DEFAULT ods_common_pkg.c_str_true,
    p_cur_busn_trx        OUT     ods_common_pkg.t_refcursor
        )
IS
    v_busn_trx_tab        t_busn_trx_tab;
    v_busn_trx_row        t_busn_trx_tab_row;

    v_cur_busn_doc_tree_man    ods_common_pkg.t_refcursor;
    v_busn_doc_tree_man_row    ods_business_doc_tree_man_pkg.t_doc_tree_man_tab_row;

    v_busn_trx_found_flag    BOOLEAN:= FALSE;

        i             PLS_INTEGER;

    v_busn_trx_table    ods_busn_trx_table_type;
        v_exist_busn_trx_ref_flag BOOLEAN:= FALSE;
        v_new_parent_busn_trx_tab_row t_busn_trx_tab_row;
        v_root_busn_doc_tree_id    ods_business_doc_tree_man.business_document_tree_id%TYPE;
    v_ref_base_busn_doc_tree_id ods_business_doc_tree_man.business_document_tree_id%TYPE;
        v_business_sequence    ods_business_transactions.business_id%TYPE;
        v_root_doc_tree_man_tab_row ods_business_doc_tree_man_pkg.t_doc_tree_man_tab_row;

BEGIN
    -- recuperamos a cadena completa desde la sequencia 1
    get_row_busn_trx_sp(
            p_business_id        => p_business_id,
            p_busn_trx_tab        => v_busn_trx_tab
            );

    -- verificamos si se deben recuperar los registros de business doc tree manual
    IF p_include_manual_flag = ods_common_pkg.c_str_true
    THEN

                -- recuperamos los registros de business doc tree manual
            ods_business_doc_tree_man_pkg.get_tree_nodes_sp(
            p_business_id    => p_business_id,
            p_cur_busn_doc_tree_man    => v_cur_busn_doc_tree_man
            );

        -- agregamos los nodos del business doc tree manual a la tabla de busn trx
                -- se prevee el caso en que la recuperacion de Business Transaction no retorne ningun registro
        i:= NVL(v_busn_trx_tab.LAST,0);

        FETCH v_cur_busn_doc_tree_man INTO v_busn_doc_tree_man_row;
        LOOP
            EXIT WHEN v_cur_busn_doc_tree_man%NOTFOUND;

            -- Agregamos el nodo a la tabla de Business Transactions
                        i:= i + 1;

                        -- transformamos el nodo business doc tree manual a business transaction
                        busn_doc_tree_man_to_trx_sp(
                                p_busn_doc_tree_man_row    => v_busn_doc_tree_man_row,
                                p_busn_trx_row        => v_busn_trx_row
                                );

                        -- verificamos si el nodo child del nodo doc tree manual esta presente en la cadena Business Transactions
            IF NOT v_exist_busn_trx_ref_flag
                        THEN
                                v_business_sequence:= find_busn_trx_node_seq_fn(
                                        p_business_id            => v_busn_trx_row.business_id,
                                        p_doc_comm_location_code    => v_busn_trx_row.doc_comm_location_code,
                                        p_document_year            => v_busn_trx_row.document_year,
                                        p_document_month            => v_busn_trx_row.document_month,
                                        p_doc_type_code                => v_busn_trx_row.doc_type_code,
                                        p_document_number            => v_busn_trx_row.document_number,
                                        p_document_item_num            => v_busn_trx_row.document_item_num,
                                        p_document_split_num            => v_busn_trx_row.document_split_num
                                        );

                IF v_business_sequence IS NOT NULL AND v_business_sequence = 1
                THEN
                                    v_exist_busn_trx_ref_flag:= TRUE;
                    v_ref_base_busn_doc_tree_id:= v_busn_doc_tree_man_row.business_document_tree_id;
                                        -- si esta presente este nodo de doc tree man pasara a reemplazar al nodo root original de Business Transaction
                                        -- almacenamos el valor de los campos manual_record_flag y integration_type originales
                                        -- del nodo root y se lo asignamos al nodo de Business Doc Tree Man
                                        v_busn_trx_row.manual_record_flag := v_busn_trx_tab(1).manual_record_flag;
                                        v_busn_trx_row.integration_type := v_busn_trx_tab(1).integration_type;
                                        v_busn_trx_row.data_source_system := v_busn_trx_tab(1).data_source_system;
                                END IF;
            END IF;


                        v_busn_trx_tab(i):= v_busn_trx_row;

            FETCH v_cur_busn_doc_tree_man INTO v_busn_doc_tree_man_row;
        END LOOP;
    END IF;

    -- verificamos si tenemos que acomodar las referencias segun la existencia o no de referencias
        -- desde business transaction a business doc tree man
    IF v_exist_busn_trx_ref_flag
    THEN
        -- se elimina ese nodo que debe ser el root de la cadena Business Transaction (sequencia 1)
                v_busn_trx_tab.DELETE(1);

                -- buscamos el nuevo nodo raiz
                v_root_busn_doc_tree_id:=  ods_business_doc_tree_man_pkg.find_doc_tree_root_doc_id_fn(
                    p_business_document_tree_id => v_ref_base_busn_doc_tree_id
                        );

        -- recuperamos los datos del nodo raiz
                ods_business_doc_tree_man_pkg.get_doc_tree_man_tab_row_sp(
                        p_business_document_tree_id    => v_root_busn_doc_tree_id,
                        p_doc_tree_man_tab_row         => v_root_doc_tree_man_tab_row
                        );

        -- armamos el nuevo nodo parent para la cadena Business Transaction
                generate_parent_busn_trx_sp(
                        p_busn_doc_tree_man_row => v_root_doc_tree_man_tab_row,
                        p_busn_trx_row => v_new_parent_busn_trx_tab_row
                        );

        -- agregamos este nuevo nodo a la cadena
                i:= v_busn_trx_tab.LAST + 1;
                v_busn_trx_tab(i):= v_new_parent_busn_trx_tab_row;

    END IF;

        -- transformamos en un ref cursor
        IF v_busn_trx_tab.COUNT > 0
        THEN
            -- transformamos a ref cursor
                p_cur_busn_trx:= get_busn_trx_row_cur_fn(
                        p_busn_trx_table    => v_busn_trx_tab
                        );
        ELSE
            -- retornamos un cursor vacio
                p_cur_busn_trx:= get_empty_busn_trx_row_cur_fn;
        END IF;
END get_business_transaction_sp;

--------------------------------------------------------------------------------

PROCEDURE get_business_transaction_sp(
    p_doc_comm_loc_code    IN ods_business_transactions.doc_comm_location_code%TYPE,
    p_document_year        IN ods_business_transactions.document_year%TYPE,
    p_document_month    IN ods_business_transactions.document_month%TYPE,
    p_doc_type_code        IN ods_business_transactions.doc_type_code%TYPE,
    p_document_number    IN ods_business_transactions.document_number%TYPE,
    p_document_item_num    IN ods_business_transactions.document_item_num %TYPE,
    p_document_split_num    IN ods_business_transactions.document_split_num%TYPE,
    p_include_manual_flag    IN ods_common_pkg.s_str_boolean,
    p_chain_type        IN ods_common_pkg.s_busn_chain_type,
    p_cur_busn_trx        OUT ods_common_pkg.t_refcursor
        )
IS
    v_business_id        ods_business_transactions.business_id%TYPE;
        v_business_sequence    ods_business_transactions.business_sequence%TYPE;

    v_business_trx_id_tab    t_business_trx_id_tab;

BEGIN

        -- si la busqueda es Build to Order, el business_id es unico
    IF p_chain_type = ods_common_pkg.c_build_to_order_type
    THEN
             -- buscamos el business_id
        get_busn_id_sp(
                      p_doc_comm_location_code=> p_doc_comm_loc_code,
                      p_document_year        => p_document_year,
                      p_document_month        => p_document_month,
                      p_doc_type_code         => p_doc_type_code,
                      p_document_number    => p_document_number,
                      p_document_item_num    => p_document_item_num,
                      p_document_split_num    => p_document_split_num,
            p_historical_flag    => ods_common_pkg.c_str_false,
                      p_business_id        => v_business_id,
                      p_business_sequence    => v_business_sequence
                      );
    ELSE
    -- si se trata de Build to Stock, el documento se podria encontrar en mas
    -- de una cadena
        -- recuperamos todos los Business_id en que se encuentra
        get_build_to_stock_busn_id_sp(
                      p_doc_comm_location_code=> p_doc_comm_loc_code,
                      p_document_year        => p_document_year,
                      p_document_month        => p_document_month,
                      p_doc_type_code         => p_doc_type_code,
                      p_document_number    => p_document_number,
                      p_document_item_num    => p_document_item_num,
                      p_document_split_num    => p_document_split_num,
            p_business_trx_id_tab   => v_business_trx_id_tab
            );

        -- verificamos si se encontro al menos un business_id
                IF v_business_trx_id_tab.COUNT > 0
                THEN
                    -- recuperamos el primero
                        v_business_id := v_business_trx_id_tab(v_business_trx_id_tab.FIRST).business_id;
                END IF;
    END IF;

    -- verificamos si se encontro la cadena
        IF v_business_id IS NULL
        THEN
            -- si no se encontro buscamos el business_id directamente en ods_business_doc_tree_man
                v_business_id:= ods_business_doc_tree_man_pkg.get_business_id_fn(
                        p_doc_comm_loc_code    => p_doc_comm_loc_code,
                        p_doc_year              => p_document_year,
                        p_doc_month             => p_document_month,
                        p_doc_type_code         => p_doc_type_code,
                        p_doc_num               => p_document_number,
                        p_doc_item_num          => p_document_item_num,
                        p_doc_split_num         => p_document_split_num
            );
    END IF;

        -- recuperamos la cadena
        get_business_transaction_sp(
                p_business_id        => v_business_id,
                p_include_manual_flag    => p_include_manual_flag,
                p_cur_busn_trx        => p_cur_busn_trx
                );

END get_business_transaction_sp;

--------------------------------------------------------------------------------
PROCEDURE get_business_transaction_sp(
        p_business_id        IN ods_business_transactions.business_id%TYPE,
    p_doc_comm_loc_code    IN ods_business_transactions.doc_comm_location_code%TYPE,
    p_document_year        IN ods_business_transactions.document_year%TYPE,
    p_document_month    IN ods_business_transactions.document_month%TYPE,
    p_doc_type_code        IN ods_business_transactions.doc_type_code%TYPE,
    p_document_number    IN ods_business_transactions.document_number%TYPE,
    p_document_item_num    IN ods_business_transactions.document_item_num%TYPE,
    p_document_split_num    IN ods_business_transactions.document_split_num%TYPE,
    p_include_manual_flag    IN ods_common_pkg.s_str_boolean,
    p_chain_type        IN ods_common_pkg.s_busn_chain_type,
    p_cur_busn_trx        OUT ods_common_pkg.t_refcursor
        )
IS
BEGIN
    -- verificamos si el parametro p_business_id tiene especificado un valor o no
        IF p_business_id IS NOT NULL
        THEN
                get_business_transaction_sp(
                        p_business_id        => p_business_id,
                        p_include_manual_flag    => p_include_manual_flag,
                        p_cur_busn_trx        => p_cur_busn_trx
                        );
        ELSE
                get_business_transaction_sp(
                        p_doc_comm_loc_code    => p_doc_comm_loc_code,
                        p_document_year        => p_document_year,
                        p_document_month    => p_document_month,
                        p_doc_type_code        => p_doc_type_code,
                        p_document_number    => p_document_number,
                        p_document_item_num    => p_document_item_num,
                        p_document_split_num    => p_document_split_num,
                        p_include_manual_flag    => p_include_manual_flag,
                        p_chain_type        => p_chain_type,
                        p_cur_busn_trx        => p_cur_busn_trx
                        );
        END IF;
END get_business_transaction_sp;

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

PROCEDURE remove_busn_class_by_bid_sp(
    p_business_id        IN    ods_business_transactions.business_id%TYPE,
        p_created_date        OUT    ods_busn_tree_classifications.created_date%TYPE,
        p_created_by        OUT    ods_busn_tree_classifications.created_by%TYPE
        )
IS

BEGIN

    DELETE    ods_busn_tree_classifications
        WHERE    business_id = p_business_id
        RETURNING created_date, created_by
        INTO    p_created_date, p_created_by;

END remove_busn_class_by_bid_sp;

--------------------------------------------------------------------------------

PROCEDURE remove_busn_class_sp
IS
        CURSOR cur_delete_busn_class
        IS
        SELECT    business_id
        FROM    ods_busn_tree_classifications btc
        WHERE    0 = 0
        AND EXISTS (SELECT    1
                FROM    ods_business_doc_tree_wrk bdtw
                    WHERE    0=0
                    AND        bdtw.business_id = btc.business_id
                );

        v_delete_busn_class_rec cur_delete_busn_class%ROWTYPE;
        v_created_date        ods_busn_tree_classifications.created_date%TYPE:=SYSTIMESTAMP;
        v_created_by        ods_busn_tree_classifications.created_by%TYPE:=c_busn_tree_classn;
BEGIN
    FOR v_delete_busn_class_rec IN cur_delete_busn_class
        LOOP
                remove_busn_class_by_bid_sp(p_business_id     => v_delete_busn_class_rec.business_id,
                                p_created_date     => v_created_date,
                                            p_created_by     => v_created_by);
        END LOOP;

END remove_busn_class_sp;

--------------------------------------------------------------------------------

PROCEDURE insert_busn_class_sp(
    --p_insert_busn_class_rec    IN t_busn_class_rec
        p_business_id            IN    ods_busn_tree_classifications.business_id%TYPE,
        p_business_typology_code    IN    ods_busn_tree_classifications.business_typology_code%TYPE,
        p_completeness_flag        IN    ods_busn_tree_classifications.completeness_flag%TYPE,
        p_broken_flag            IN    ods_busn_tree_classifications.broken_flag%TYPE,
        p_finishing_order_key        IN    ods_busn_tree_classifications.finishing_order_key%TYPE,
        p_conforming_order_key        IN    ods_busn_tree_classifications.conforming_order_key%TYPE,
        p_finshing_process_classn_code    IN    ods_busn_tree_classifications.finishing_process_classn_code%TYPE,
        p_tenaris_conforming_mill_flag    IN    ods_busn_tree_classifications.tenaris_conforming_mill_flag%TYPE,
        p_completed_business_ref_id    IN    ods_busn_tree_classifications.completed_business_ref_id%TYPE,
        p_data_source_system        IN    ods_busn_tree_classifications.data_source_system%TYPE,
        p_process_date            IN    ods_busn_tree_classifications.process_date%TYPE,
        p_created_date            IN    ods_busn_tree_classifications.created_date%TYPE,
        p_created_by            IN    ods_busn_tree_classifications.created_by%TYPE,
        p_finishing_flag            IN    ods_busn_tree_classifications.finishing_reference_flag%TYPE,
        p_conforming_flag            IN    ods_busn_tree_classifications.conforming_reference_flag%TYPE,
        p_historical_chain_flag            IN    ods_busn_tree_classifications.historical_chain_flag%TYPE,
        p_finishing_plant_code		IN ods_busn_tree_classifications.finishing_plant_code%TYPE )
IS
BEGIN
        INSERT INTO ods_busn_tree_classifications(
                business_id                    ,
                business_typology_code        ,
                completeness_flag        ,
                broken_flag            ,
                finishing_order_key        ,
                conforming_order_key        ,
                finishing_process_classn_code    ,
                tenaris_conforming_mill_flag    ,
                completed_business_ref_id    ,
                data_source_system        ,
                process_date            ,
                created_date            ,
                created_by            ,
                last_updated_date        ,
                last_updated_by        ,
                finishing_reference_flag        ,
                conforming_reference_flag	,
                historical_chain_flag	,
                finishing_plant_code
                )
        VALUES (
                p_business_id            ,
                p_business_typology_code    ,
                p_completeness_flag        ,
                p_broken_flag            ,
                p_finishing_order_key        ,
                p_conforming_order_key        ,
                p_finshing_process_classn_code    ,
                p_tenaris_conforming_mill_flag    ,
                p_completed_business_ref_id    ,
                p_data_source_system        ,
                p_process_date            ,
                NVL(p_created_date,SYSTIMESTAMP),
                NVL(p_created_by,c_busn_tree_classn),
                SYSTIMESTAMP            ,
                c_busn_tree_classn        ,
                NVL(p_finishing_flag,'N')        ,  --Added NVL ITTEN00269101
                NVL(p_conforming_flag,'N')	,
                p_historical_chain_flag ,         --Added NVL ITTEN00269101
                p_finishing_plant_code
                );

END /* Formatted on 20/01/2014 06:16:22 p.m. (QP5 v5.149.1003.31008) */
insert_busn_class_sp;

--------------------------------------------------------------------------------

PROCEDURE classify_business_trx_sp(
        p_business_id        IN ods_business_transactions.business_id%TYPE,
        p_data_source_system    IN ods_business_transactions.data_source_system%TYPE
        )
IS
        v_busn_trx_data_table        ods_busn_trx_table_type;
        v_business_id            ods_busn_tree_classifications.business_id%TYPE;
        v_business_typology_code    ods_busn_tree_classifications.business_typology_code%TYPE;
        v_historical_chain_flag		ods_busn_tree_classifications.historical_chain_flag%TYPE;
        v_completeness_flag        ods_busn_tree_classifications.completeness_flag%TYPE;
        v_broken_flag            ods_busn_tree_classifications.broken_flag%TYPE;
        v_finishing_order_key        ods_busn_tree_classifications.finishing_order_key%TYPE;
        v_conforming_order_key        ods_busn_tree_classifications.conforming_order_key%TYPE;
        v_finshing_process_classn_code    ods_busn_tree_classifications.finishing_process_classn_code%TYPE;
        v_tenaris_conforming_mill_flag    ods_busn_tree_classifications.tenaris_conforming_mill_flag%TYPE;
        v_completed_business_ref_id    ods_busn_tree_classifications.completed_business_ref_id%TYPE;
        v_data_source_system        ods_busn_tree_classifications.data_source_system%TYPE;
        v_process_date            ods_busn_tree_classifications.process_date%TYPE;
        v_created_date            ods_busn_tree_classifications.created_date%TYPE;
        v_created_by            ods_busn_tree_classifications.created_by%TYPE;
        v_finishing_flag            ods_busn_tree_classifications.finishing_reference_flag%TYPE;
        --
        v_finishing_plant			ods_busn_tree_classifications.finishing_plant_code%TYPE;
        --
        v_conforming_flag            ods_busn_tree_classifications.conforming_reference_flag%TYPE;
        v_new_created_date            ods_busn_tree_classifications.created_date%TYPE;
        v_code	NUMBER;
        v_errm	VARCHAR2(250);
        v_process	VARCHAR2(50);
BEGIN

        v_process := 'RETRIEVE_BUSN_TRX_DATA_SP';

       -- cargamos los datos del Business Transaction
        retrieve_busn_trx_data_sp (
                p_business_id        => p_business_id,
                p_busn_trx_data_table     => v_busn_trx_data_table
                );

        v_process := 'REMOVE_BUSN_CLASS_BY_BID_SP';

        --Borramos la clasificacion de una cadena en la ODS_BUSN_TREE_CLASSIFICATIONS
        remove_busn_class_by_bid_sp (p_business_id     => p_business_id,
                         p_created_date     => v_created_date,
                                     p_created_by     => v_created_by);

        --Recuperamos la fecha que se insertará como CREATED_DATE de la cadena
        --en la tabla ODS_BUSN_TREE_CLASSIFICATIONS (Ticket ITTEN00264349)
	IF SQL%ROWCOUNT = 0
        THEN
		v_process := 'GET_BUSN_TRX_CREATED_DATE_FN';

                --dbms_output.PUT_LINE('get_busn_trx_created_date_fn'||' - '||p_business_id);
                --
        	v_new_created_date := get_busn_trx_created_date_fn (p_busn_trx_data_table     => v_busn_trx_data_table);
                --
        	IF	v_new_created_date IS NOT NULL
        	THEN
        		v_created_date := v_new_created_date;
        	END IF;
        END IF;
        --
        v_process := 'SET_HISTORICAL_CHAIN_FLAG_FN';

        --Verificamos si la cadena es historica o no.
        v_historical_chain_flag := set_historical_chain_flag_fn(v_busn_trx_data_table);
        --
        v_process := 'GET_BUSN_TYPOLOGY_FN';

        --Definimos la tipologia del Business
        v_business_typology_code := get_busn_typology_fn(v_busn_trx_data_table);
        --
        v_process :='CALC_STUTUS_CLASS_SP';

        --Definimos el estado de completitud
        calc_stutus_class_sp (p_busn_trx_data_table     => v_busn_trx_data_table,
                              p_typology_code        => v_business_typology_code,
                              p_historical_chain_flag        => v_historical_chain_flag,
                              p_completeness_flag    => v_completeness_flag,
                              p_broken_flag        => v_broken_flag,
                              p_ten_conf_mill_flag    => v_tenaris_conforming_mill_flag,
                              p_business_ref_id        => v_completed_business_ref_id
                              );
        --
        v_process := 'CALC_FINISHING_ORDER_SP';

        --Definimos la orden de Finishing
        calc_finishing_order_sp (
                                p_busn_trx_data_table           => v_busn_trx_data_table,
                                p_typology_code                 => v_business_typology_code,
                                p_historical_chain_flag         => v_historical_chain_flag,
                                p_finishing_order_key           => v_finishing_order_key,
                                p_finishing_process_code        => v_finshing_process_classn_code,
                                p_finishing_flag 		=> v_finishing_flag
                                );
        --
        v_process := 'CALC_FINISHING_PLANT_SP';

        --Definimos la Planta de Finishing
        calc_finishing_plant_sp (
                                p_busn_trx_data_table  => v_busn_trx_data_table,
                                p_finishing_order_key  => v_finishing_order_key,
                      		p_finishing_plant      => v_finishing_plant
                                );
        --
        v_process := 'CALC_CONFORMING_ORDER_SP';

        --Definimos la orden de Conforming
        calc_conforming_order_sp (p_busn_trx_data_table     => v_busn_trx_data_table,
                      			  p_completeness_flag        => v_completeness_flag,
								  p_historical_chain_flag      => v_historical_chain_flag,
                                  p_conforming_order_key    => v_conforming_order_key,
                                  p_conforming_flag    => v_conforming_flag
                                  );
        --
        v_process := 'INSERT_BUSN_CLASS_SP';

        --Generamos la clasificacion de una cadena
        insert_busn_class_sp (p_business_id            => p_business_id,
                              p_business_typology_code        => v_business_typology_code,
                              p_completeness_flag        => v_completeness_flag,
                              p_broken_flag            => v_broken_flag,
                              p_finishing_order_key        => v_finishing_order_key,
                              p_conforming_order_key        => v_conforming_order_key,
                              p_finshing_process_classn_code    => v_finshing_process_classn_code,
                              p_tenaris_conforming_mill_flag    => v_tenaris_conforming_mill_flag,
                              p_completed_business_ref_id    => v_completed_business_ref_id,
                              p_data_source_system        => p_data_source_system,
                              p_process_date            => TRUNC(SYSDATE),
                              p_created_date            => v_created_date,
                              p_created_by            => v_created_by,
                              p_finishing_flag            => v_finishing_flag,
                              p_conforming_flag            => v_conforming_flag,
                              p_historical_chain_flag            => v_historical_chain_flag,
                              p_finishing_plant_code		=> v_finishing_plant
                              );
        --
        COMMIT;
        --
	--Almacenamos en una variable interna del Package, el estado de completitud del Business, para luego referenciarlo
        --desde el SP reclassify_list_busn_trx_sp
	v_g_completeness_flag:= v_completeness_flag;

EXCEPTION
        WHEN OTHERS THEN
                ROLLBACK;
                v_errm := SUBSTR(SQLERRM, 1 , 250);
		ODS_ERROR_LOG_PKG.INSERT_ERROR_LOG_TABLE (
                        'CLASSIFY_BUSINESS_TRX_SP'||' - '||v_process,
                        v_errm,
                        p_business_id,
                        'TENARIS',
                        SYSDATE,
                        'TENARIS');

END classify_business_trx_sp;

--------------------------------------------------------------------------------


PROCEDURE classify_list_business_trx_sp
IS

l_exst number(1) default 0;

    CURSOR cur_busn_trx_wrk -- CAMBIO EN ITTEN00371400 30/06/2016.
        IS -- PRIMERO NOS TRAEMOS TODAS LAS NOVEDADES DIARIAS PARA PROCESAR LAS CADENAS DESDE LA WORK.
        SELECT business_id, created_by
          FROM ods_business_doc_tree_wrk
        UNION -- LUEGO NOS TRAEMOS TODAS LAS COMPRAS DE TERCERO QUE PERTENECEN A CADENAS ROTAS
        -- LAS CUALES LLEGARON LUEGO DE LA ULTIMA RECLASIFICACION DEL NEGOCIO DEJANDO BROKEN A LA CADENA
        SELECT c.business_id, c.created_by
          FROM ods_busn_tree_classifications c,
               ods_business_transactions     t,
               ods_purchase_orders_hdr       h,
               ods_subjects             s
         where c.broken_flag = 'Y'
           and t.business_id = c.business_id
           and t.doc_portfolio_type = '01'
           and t.doc_comm_location_code = h.commercial_location_code
           and t.document_year = h.po_order_year
           and t.document_month = h.po_order_month
           and t.doc_type_code = h.doc_type_code
           and t.document_number = h.po_number
           and h.supplier_subject_code = s.c_subject_id
           and s.c_asosocten IS NULL
           and h.created_date > c.last_updated_date;

        v_busn_trx_wrk_rec cur_busn_trx_wrk%ROWTYPE;
BEGIN
    FOR v_busn_trx_wrk_rec IN cur_busn_trx_wrk
        LOOP
			--dbms_output.PUT_LINE('business_id: '||to_char(v_busn_trx_wrk_rec.business_id));
     SELECT NVL(MAX(1), 0)
       into l_exst
       FROM ods_business_doc_tree_wrk ic
      WHERE ic.business_id = v_busn_trx_wrk_rec.business_id;

    IF l_exst = 0 THEN --- SI NO EXISTE EN LA WORK SE INSERTA PARA QUE LUEGO SUBA AL DWT PARA CLASIFICARSE
      ods_business_doc_tree_pkg.insert_busn_doc_tree_wrk_sp(
                        p_business_id   => v_busn_trx_wrk_rec.business_id,
                        p_created_by    => v_busn_trx_wrk_rec.created_by
                        );
                        END IF;

            classify_business_trx_sp(
                p_business_id           => v_busn_trx_wrk_rec.business_id,
                p_data_source_system    => v_busn_trx_wrk_rec.created_by
                );

        END LOOP;

END classify_list_business_trx_sp;

--------------------------------------------------------------------------------


FUNCTION set_historical_chain_flag_fn (
	p_busn_trx_data_table IN ods_busn_trx_table_type)
	RETURN ods_busn_tree_classifications.historical_chain_flag%TYPE
IS
	v_cant					  NUMBER := 0;
	v_historical_chain_flag   ods_busn_tree_classifications.historical_chain_flag%TYPE;
	v_busn_trx_data_table	  ods_busn_trx_table_type
								  := ods_busn_trx_table_type ();
BEGIN
	--
	v_busn_trx_data_table := p_busn_trx_data_table;
	--
    BEGIN
        SELECT COUNT (1)
          INTO v_cant
          FROM TABLE (CAST (v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table
         WHERE 0 = 0
         AND ods_busn_trx_table.historical_link_flag = 'Y';
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
        	v_cant := 0;
	END;

	IF v_cant = v_busn_trx_data_table.COUNT
	THEN
		v_historical_chain_flag := 'Y';
	ELSE
		v_historical_chain_flag := 'N';
	END IF;
	--
	RETURN v_historical_chain_flag;
--
END set_historical_chain_flag_fn;

--------------------------------------------------------------------------------

FUNCTION get_busn_typology_fn (
                p_busn_trx_data_table     IN    ods_busn_trx_table_type)
RETURN ods_business_typologies.business_typology_code%TYPE
IS
        v_typology        ods_business_typologies.business_typology_code%TYPE;
        v_busn_trx_data_table    ods_busn_trx_table_type:= ods_busn_trx_table_type();

BEGIN
        --
        v_busn_trx_data_table := p_busn_trx_data_table;
        --
        BEGIN
                SELECT  'BRT'
                INTO    v_typology
                FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table
                WHERE   0=0
                AND     ROWNUM = 1
                AND     doc_portfolio_type = '90'
                AND     (    secondary_costing_flag = ods_common_pkg.c_str_no
                         OR (secondary_costing_flag = ods_common_pkg.c_str_yes AND doc_portfolio_type = '40')
                        )
                AND EXISTS (SELECT  1
                            FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                            WHERE   0=0
                            AND     ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                            AND     ods_busn_trx_table_aux.business_sequence =  ods_busn_trx_table.business_sequence + 1
                            AND     ods_busn_trx_table_aux.doc_portfolio_type = '02'
                            AND (   ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                 OR(ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                )
                            AND    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                           );			--ITTEN00275682

        EXCEPTION
                WHEN NO_DATA_FOUND THEN
                        BEGIN
                                SELECT    'BSR'
                                INTO    v_typology
                                FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table
                                WHERE   0=0
                                AND     ROWNUM = 1
                                AND     (    secondary_costing_flag = ods_common_pkg.c_str_no
                                         OR (secondary_costing_flag = ods_common_pkg.c_str_yes AND doc_portfolio_type = '40')
                                        )
                                AND((     doc_portfolio_type = '90'
                                     AND  EXISTS (SELECT 1
                                                  FROM     TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                  WHERE     0=0
                                                  AND     ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                  AND     ods_busn_trx_table_aux.business_sequence = ods_busn_trx_table.business_sequence + 1
                                                  AND     ods_busn_trx_table_aux.doc_portfolio_type <> '02'
                                                  AND (   ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                       OR(ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                      )
                                                 )			--ITTEN00275682
                                    )
                                    OR   (    business_sequence = 1
                                           AND doc_portfolio_type IN ('01','06')
                                         )
                                    )
                                ;			--ITTEN00275682

                        EXCEPTION
                                WHEN NO_DATA_FOUND THEN
                                        BEGIN
                                                SELECT    'BOA'
                                                INTO    v_typology
                                                FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table
                                                WHERE    0=0
                                                AND    ROWNUM = 1
                                                AND    doc_portfolio_type = '50'
                                                AND    (    secondary_costing_flag = ods_common_pkg.c_str_no
                                                        OR (secondary_costing_flag = ods_common_pkg.c_str_yes AND doc_portfolio_type = '40')
                                                       )
                                                AND EXISTS (SELECT   1
                                                            FROM     TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                            WHERE    0=0
                                                            AND      ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                            AND      ods_busn_trx_table_aux.business_sequence = ods_busn_trx_table.business_sequence + 1
                                                            AND      ods_busn_trx_table_aux.doc_portfolio_type = '02'
                                                            AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                 OR (ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                )
                                                           )				--ITTEN00275682
                                                AND NOT EXISTS (SELECT    1
                                                                FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                WHERE    0=0
                                                                AND    ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                AND    ods_busn_trx_table_aux.business_reference_id IS NOT NULL
                                                                AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                     OR(ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                    )
                                                                );			--ITTEN00275682

                                        EXCEPTION
                                                WHEN NO_DATA_FOUND THEN
                                                        BEGIN
                                                                SELECT    'SAP'
                                                                INTO    v_typology
                                                                FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table
                                                                WHERE    0=0
                                                                AND    ROWNUM = 1
                                                                AND    doc_portfolio_type = '05'
                                                                AND    (    secondary_costing_flag = ods_common_pkg.c_str_no
                                                                        OR    (secondary_costing_flag = ods_common_pkg.c_str_yes AND doc_portfolio_type = '40')
                                                                       )
                                                                AND EXISTS (SELECT    1
                                                                            FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                            WHERE    0=0
                                                                            AND        ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                            AND         ods_busn_trx_table_aux.business_sequence = ods_busn_trx_table.business_sequence + 1
                                                                            AND        ods_busn_trx_table_aux.doc_portfolio_type = '02'
                                                                            AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                 OR(    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                                )
                                                                            )			--ITTEN00275682
                                                                AND EXISTS (SELECT    1
                                                                            FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                            WHERE    0=0
                                                                            AND        ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                            AND         ods_busn_trx_table_aux.business_sequence = ods_busn_trx_table.business_sequence + 2
                                                                            AND        ods_busn_trx_table_aux.doc_portfolio_type = '04'
                                                                            AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                 OR(    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                                )
                                                                            )				--ITTEN00275682
                                                                AND EXISTS (SELECT    1
                                                                            FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                            WHERE    0=0
                                                                            AND        ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                            AND         ods_busn_trx_table_aux.business_sequence = ods_busn_trx_table.business_sequence + 3
                                                                            AND        ods_busn_trx_table_aux.business_reference_id IS NOT NULL
                                                                            AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                 OR(    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                                )
                                                                            );			--ITTEN00275682

                                                        EXCEPTION
                                                                WHEN NO_DATA_FOUND THEN
                                                                        BEGIN
                                                                                SELECT    'SSP'
                                                                                INTO    v_typology
                                                                                FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table
                                                                                WHERE    0=0
                                                                                AND    ROWNUM = 1
                                                                                AND    doc_portfolio_type = '02'
                                                                                AND    (    secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                        OR    (secondary_costing_flag = ods_common_pkg.c_str_yes AND doc_portfolio_type = '40')
                                                                                       )
                                                                                --AND    historical_link_flag = ods_common_pkg.c_str_no			ITTEN00275682
                                                                                AND EXISTS (SELECT    1
                                                                                            FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                                            WHERE    0=0
                                                                                            AND        ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                                            AND         ods_busn_trx_table_aux.business_sequence = ods_busn_trx_table.business_sequence + 1
                                                                                            AND        ods_busn_trx_table_aux.doc_portfolio_type = '04'
                                                                                            AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                                 OR(    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                                                )
                                                                                           )			--ITTEN00275682
                                                                                AND EXISTS (SELECT    1
                                                                                            FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                                            WHERE    0=0
                                                                                            AND        ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                                            AND         ods_busn_trx_table_aux.business_sequence = ods_busn_trx_table.business_sequence + 2
                                                                                            AND        ods_busn_trx_table_aux.business_reference_id IS NOT NULL
                                                                                            AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                                 OR(    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                                                )
                                                                                            );			--ITTEN00275682

                                                                        EXCEPTION
                                                                                WHEN NO_DATA_FOUND THEN
                                                                                        BEGIN
                                                                                                SELECT    'BOS'
                                                                                                INTO    v_typology
                                                                                                FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table
                                                                                                WHERE    0=0
                                                                                                AND    ROWNUM = 1
                                                                                                AND    doc_portfolio_type = '02'
                                                                                                AND    (    secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                                        OR    (secondary_costing_flag = ods_common_pkg.c_str_yes AND doc_portfolio_type = '40')
                                                                                                        )
                                                                                                AND NOT EXISTS (SELECT    1
                                                                                                                FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                                                                WHERE    0=0
                                                                                                                AND    ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                                                                AND    ods_busn_trx_table_aux.doc_portfolio_type = '50'
                                                                                                                AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                                                     OR(ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                                                                    )
                                                                                                                )		--ITTEN00275682
                                                                                                AND EXISTS (SELECT    1
                                                                                                            FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                                                            WHERE    0=0
                                                                                                            AND        ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                                                            AND        ods_busn_trx_table_aux.business_reference_id IS NULL
                                                                                                            AND        ods_busn_trx_table_aux.business_sequence = 1
                                                                                                            AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                                                 OR(    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                                                                )
                                                                                                           )			--ITTEN00275682
                                                                                                AND EXISTS (SELECT    1
                                                                                                            FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                                                            WHERE    0=0
                                                                                                            AND        ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                                                            AND        ods_busn_trx_table_aux.business_reference_id IS NULL
                                                                                                            AND        ods_busn_trx_table_aux.business_sequence = 2
                                                                                                            AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                                                 OR(    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                                                                )
                                                                                                           );			--ITTEN00275682

                                                                                        EXCEPTION
                                                                                                WHEN NO_DATA_FOUND THEN
                                                                                                        BEGIN
                                                                                                                SELECT    'BSS'
                                                                                                                INTO    v_typology
                                                                                                                FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table
                                                                                                                WHERE    0=0
                                                                                                                AND    ROWNUM = 1
                                                                                                                AND    doc_portfolio_type = '02'
                                                                                                                AND    (    secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                                                        OR    (secondary_costing_flag = ods_common_pkg.c_str_yes AND doc_portfolio_type = '40')
                                                                                                                       )
                                                                                                                AND EXISTS (SELECT    1
                                                                                                                            FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                                                                            WHERE    0=0
                                                                                                                            AND        ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                                                                            AND        ods_busn_trx_table_aux.business_reference_id IS NOT NULL
                                                                                                                            AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                                                                 OR(    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                                                                                )
                                                                                                                           );			--ITTEN00275682

                                                                                                        EXCEPTION
                                                                                                                WHEN NO_DATA_FOUND THEN
                                                                                                                        BEGIN
                                                                                                                                SELECT    'SRV'
                                                                                                                                INTO    v_typology
                                                                                                                                FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table
                                                                                                                                WHERE    0=0
                                                                                                                                AND    ROWNUM = 1
                                                                                                                                AND    (    secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                                                                        OR    (secondary_costing_flag = ods_common_pkg.c_str_yes AND doc_portfolio_type = '40')
                                                                                                                                        )
                                                                                                                                AND EXISTS (SELECT    1
                                                                                                                                            FROM    TABLE(CAST(v_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                                                                                                            WHERE   0=0
                                                                                                                                            AND        ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id
                                                                                                                                            AND        ods_busn_trx_table_aux.doc_portfolio_type = '41'
                                                                                                                                            AND (    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_no
                                                                                                                                                 OR(    ods_busn_trx_table_aux.secondary_costing_flag = ods_common_pkg.c_str_yes AND ods_busn_trx_table_aux.doc_portfolio_type = '40')
                                                                                                                                                )
                                                                                                                                           );			--ITTEN00275682

                                                                                                                        EXCEPTION
                                                                                                                                WHEN NO_DATA_FOUND THEN
                                                                                                                                        v_typology := '999';

                                                                                                                        END;
                                                                                                        END;
                                                                                        END;
                                                                        END;
                                                        END;
                                        END;
                        END;
        END;
        --
        RETURN v_typology;
        --

END get_busn_typology_fn;

--------------------------------------------------------------------------------

FUNCTION is_supplier_not_tenaris_fn (--p_busn_trx_data_table     IN    ods_busn_trx_table_type
                     p_doc_comm_loc     IN    ods_business_transactions.doc_comm_location_code%TYPE,
                     p_doc_num        IN    ods_business_transactions.document_number%TYPE,
                                     p_doc_month    IN    ods_business_transactions.document_month%TYPE,
                                     p_doc_year        IN    ods_business_transactions.document_year%TYPE,
                                     p_doc_type        IN    ods_business_transactions.doc_type_code%TYPE
                    )
RETURN VARCHAR2
IS
    v_result    ods_common_pkg.s_str_yes_no;
BEGIN
    BEGIN
        SELECT     ods_common_pkg.c_str_yes
                INTO    v_result
                FROM       ods_purchase_orders_hdr h,
                        ods_subjects s
                WHERE      0=0
                AND    h.commercial_location_code = p_doc_comm_loc
                AND        h.po_number = p_doc_num
                AND        h.po_order_month = p_doc_month
                AND        h.po_order_year = p_doc_year
                AND        h.doc_type_code = p_doc_type
                AND        s.c_subject_id = h.supplier_subject_code
                AND        s.c_asosocten IS NULL
                AND    ROWNUM = 1;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                    v_result := ods_common_pkg.c_str_no;
        END;
        --
        RETURN v_result;
        --
END is_supplier_not_tenaris_fn;

--------------------------------------------------------------------------------

FUNCTION exist_delivery_fn (p_doc_comm_loc     IN    ods_business_transactions.doc_comm_location_code%TYPE,
                     p_doc_num        IN    ods_business_transactions.document_number%TYPE,
                                     p_doc_month    IN    ods_business_transactions.document_month%TYPE,
                                     p_doc_year        IN    ods_business_transactions.document_year%TYPE,
                                     p_doc_type        IN    ods_business_transactions.doc_type_code%TYPE,
                                     p_doc_itm        IN    ods_business_transactions.document_item_num%TYPE,
                                     p_doc_spl        IN    ods_business_transactions.document_split_num%TYPE
                    )
RETURN VARCHAR2
IS
    v_result    ods_common_pkg.s_str_yes_no:= ods_common_pkg.c_str_yes;
        v_dummy        PLS_INTEGER;
BEGIN
    BEGIN
        SELECT  1
                INTO    v_dummy
                FROM    ods_deliveries d
                WHERE   d.so_commercial_location_code = p_doc_comm_loc
                AND     d.so_doc_type_code = p_doc_type
                AND     d.so_order_year = p_doc_year
                AND     d.so_order_month = p_doc_month
                AND     d.so_number = p_doc_num
                AND     d.so_item_num = p_doc_itm
                AND     d.so_split_num = p_doc_spl
                AND    ROWNUM = 1;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                    v_result := ods_common_pkg.c_str_no;
        END;
        --
        RETURN v_result;
        --
END exist_delivery_fn;

--------------------------------------------------------------------------------

FUNCTION exist_stock_fn (p_doc_comm_loc     IN    ods_business_transactions.doc_comm_location_code%TYPE,
             p_doc_num        IN    ods_business_transactions.document_number%TYPE,
                         p_doc_month        IN    ods_business_transactions.document_month%TYPE,
                         p_doc_year        IN    ods_business_transactions.document_year%TYPE,
                         p_doc_type        IN    ods_business_transactions.doc_type_code%TYPE,
                         p_doc_itm        IN    ods_business_transactions.document_item_num%TYPE,
                         p_doc_spl        IN    ods_business_transactions.document_split_num%TYPE,
                         p_doc_portfolio    IN    ods_business_transactions.doc_portfolio_type%TYPE
                    )
RETURN VARCHAR2
IS
    v_result    ods_common_pkg.s_str_yes_no:= ods_common_pkg.c_str_yes;
        v_dummy        PLS_INTEGER;
BEGIN
    IF p_doc_portfolio = '90' THEN
            BEGIN
                    SELECT  1
                    INTO    v_dummy
                    FROM    ods_stock_rfd s
                    WHERE   s.ro_commercial_location_code = p_doc_comm_loc
                    AND     s.ro_doc_type_code = p_doc_type
                    AND     s.ro_order_year = p_doc_year
                    AND     s.ro_order_month = p_doc_month
                    AND     s.ro_number = p_doc_num
                    AND     s.ro_item_num = p_doc_itm
                    AND     s.ro_split_num = p_doc_spl
                    AND    ROWNUM = 1;
            EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                            v_result := ods_common_pkg.c_str_no;
            END;

        ELSIF p_doc_portfolio = '50' THEN
            BEGIN
                    SELECT  1
                    INTO    v_dummy
                    FROM    ods_stock_rfd s
                    WHERE   s.agreement_doc_type_code = p_doc_type
                    AND     s.agreement_year = p_doc_year
                    AND     s.agreement_number = p_doc_num
                    AND     s.agreement_item_num = p_doc_itm
                    AND    ROWNUM = 1;
            EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                            v_result := ods_common_pkg.c_str_no;
            END;
        ELSIF p_doc_portfolio = '06' THEN
            BEGIN
                    SELECT  1
                    INTO    v_dummy
                    FROM    ods_stock_rfd s
                    WHERE   s.pro_commercial_location_code = p_doc_comm_loc
                    AND     s.pro_doc_type_code = p_doc_type
                    AND     s.pro_order_year = p_doc_year
                    AND     s.pro_order_month = p_doc_month
                    AND     s.pro_number = p_doc_num
                    AND     s.pro_item_num = p_doc_itm
                    AND     s.pro_split_num = p_doc_spl
                    AND    ROWNUM = 1;
            EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                            v_result := ods_common_pkg.c_str_no;
            END;
        ELSIF p_doc_portfolio = '01' THEN
            BEGIN
                    SELECT  1
                    INTO    v_dummy
                    FROM    ods_stock_rfd s
                    WHERE   s.po_commercial_location_code = p_doc_comm_loc
                    AND     s.po_doc_type_code = p_doc_type
                    AND     s.po_order_year = p_doc_year
                    AND     s.po_order_month = p_doc_month
                    AND     s.po_number = p_doc_num
                    AND     s.po_item_num = p_doc_itm
                    AND     s.po_split_num = p_doc_spl
                    AND    ROWNUM = 1;
            EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                            v_result := ods_common_pkg.c_str_no;
            END;
            --
        END IF;
        --
        RETURN v_result;
        --
END exist_stock_fn;

--------------------------------------------------------------------------------

PROCEDURE calc_stutus_class_sp (p_busn_trx_data_table     IN    ods_busn_trx_table_type,
                p_typology_code        IN    ods_busn_tree_classifications.business_typology_code%TYPE,
				p_historical_chain_flag   IN	 ods_busn_tree_classifications.historical_chain_flag%TYPE,
                p_completeness_flag    OUT    ods_busn_tree_classifications.completeness_flag%TYPE,
                                p_broken_flag        OUT    ods_busn_tree_classifications.broken_flag%TYPE,
                                p_ten_conf_mill_flag    OUT    ods_busn_tree_classifications.tenaris_conforming_mill_flag%TYPE,
                                p_business_ref_id    OUT    ods_busn_tree_classifications.business_id%TYPE
                               )
IS
    	v_dummy            PLS_INTEGER;
        v_completeness_flag    ods_busn_tree_classifications.completeness_flag%TYPE;
        v_broken_flag        ods_busn_tree_classifications.broken_flag%TYPE;
        v_ten_conf_mill_flag    ods_busn_tree_classifications.tenaris_conforming_mill_flag%TYPE;
        v_business_reference_id    ods_busn_tree_classifications.business_id%TYPE;
        v_portfolio_1        ods_common_pkg.s_str_yes_no;
        v_business_sequence_01		ods_business_transactions.business_sequence%TYPE;
        v_business_sequence_88		ods_business_transactions.business_sequence%TYPE;
        v_flag      varchar2(20);
        v_errm	VARCHAR2(64);
        query1  varchar2 (4000);
        query2  varchar2 (4000);
        query3  varchar2 (4000);
        query4  varchar2 (4000);
        query5  varchar2 (4000);
        query6  varchar2 (4000);
        query7  varchar2 (4000);
        query8  varchar2 (4000);
        query9  varchar2 (4000);
        query10  varchar2 (4000);
        query11  varchar2 (4000);
        query12  varchar2 (4000);
        query13  varchar2 (4000);
        condicion varchar2 (100);
--
BEGIN
    --
        IF p_historical_chain_flag = 'Y' THEN
                condicion := ' AND ''N'' = ''N''';
        ELSE
                condicion := ' AND historical_link_flag = ''N''';
        END IF;
		--
    	v_business_sequence_01 := 0;
        v_business_sequence_88 := 0;
        v_flag := '1';

        IF p_typology_code IN /*('BSA','BSK','BSS')*/ ('BOA', 'BSS','BOS','BRT','SAP','SSP') THEN

                IF p_typology_code IN ('BOA', 'BOS')  THEN		--ITTEN00264345

                        BEGIN
                                v_flag := '2';

                                query1 := 'SELECT b.business_sequence
                                  FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) b
                                 WHERE 0 = 0
                                       AND b.business_sequence =
                                               (SELECT MIN (a.business_sequence)
                                                  FROM TABLE (
                                                           CAST (
                                                               :v_tabla AS ods_busn_trx_table_type)) a
                                                 WHERE 0 = 0
                                                       AND (a.business_reference_id = 0
                                                            OR a.business_reference_id IS NULL)
                                                       AND a.doc_portfolio_type = ''88''';
									--
                                    query2 := ')
                                      + 1
                                    AND b.doc_portfolio_type = ''01''
                                    AND b.parent_doc_portfolio_type = ''06''
                                    AND (b.business_reference_id = 0 OR b.business_reference_id IS NULL)';

                                query1 := query1 || condicion || query2;

                                EXECUTE IMMEDIATE (query1)
                                INTO v_business_sequence_01
                                USING p_busn_trx_data_table, p_busn_trx_data_table;
                        --
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                    v_business_sequence_01 := 0;
                        END;

                        IF v_business_sequence_01 <> 0 THEN
                                v_ten_conf_mill_flag := ods_common_pkg.c_str_yes;
                                v_broken_flag := ods_common_pkg.c_str_no;
                                v_flag := '3';

                                BEGIN
                                        query3 := 'SELECT c.business_sequence
                                          FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) c
                                         WHERE	   0 = 0
                                               AND c.business_sequence > :v_business_sequence_01
                                               AND c.doc_portfolio_type = ''88''
                                               AND (c.business_reference_id = 0 OR c.business_reference_id IS NULL)
                                               AND ROWNUM = 1';

                                        query3 := query3 || condicion;

                                        EXECUTE IMMEDIATE (query3)
                                        INTO v_business_sequence_88
                                        USING p_busn_trx_data_table, v_business_sequence_01;
                                --
                                EXCEPTION
                                    WHEN NO_DATA_FOUND THEN
                                    		v_business_sequence_88 := 0;
                                            v_completeness_flag := ods_common_pkg.c_str_no;
                                END;

                                IF v_business_sequence_88 <> 0 THEN
                                        v_completeness_flag := ods_common_pkg.c_str_yes;
                                END IF;

                        END IF;

				END IF;

                IF v_business_sequence_01 = 0 THEN

                        BEGIN
                            	v_flag := '4';

                                query4 := 'SELECT ''Y'', business_reference_id
                                  FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
                                 WHERE 0 = 0 AND doc_portfolio_type = ''88''
                                       AND (secondary_costing_flag = ''N''
                                            OR (secondary_costing_flag = ''Y''
                                                AND doc_portfolio_type = ''40''))
                                       AND ROWNUM = 1';

                                query4 :=  query4 || condicion;

                                EXECUTE IMMEDIATE (query4)
                                INTO v_ten_conf_mill_flag, v_business_reference_id
                                USING p_busn_trx_data_table;
                        --
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                    v_ten_conf_mill_flag := ods_common_pkg.c_str_no;
                                        v_business_reference_id := NULL;
                        END;

                        IF v_ten_conf_mill_flag = ods_common_pkg.c_str_no THEN
                            --
                                BEGIN
                                        v_flag := '5';

                                        query5 := 'SELECT ''Y'', business_reference_id
                                          FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
                                         WHERE 0 = 0 AND ods_busn_trx_table.doc_portfolio_type = ''01''
                                               AND (secondary_costing_flag = ''N''
                                                    OR (secondary_costing_flag = ''Y''
                                                        AND doc_portfolio_type = ''40''))
                                               AND ODS_BUSINESS_TRANSACTION_PKG.is_supplier_not_tenaris_fn (
                                                   ods_busn_trx_table.doc_comm_location_code,
                                                   ods_busn_trx_table.document_number,
                                                   ods_busn_trx_table.document_month,
                                                   ods_busn_trx_table.document_year,
                                                   ods_busn_trx_table.doc_type_code) = ''Y''
                                               AND ROWNUM = 1';

                                        query5 := query5 || condicion;

                                        EXECUTE IMMEDIATE (query5)
                                        INTO v_portfolio_1, v_business_reference_id
                                        USING p_busn_trx_data_table;
                                --
                                EXCEPTION
                                        WHEN NO_DATA_FOUND THEN
                                                 v_portfolio_1 := ods_common_pkg.c_str_no;
                                                 v_business_reference_id := NULL;
                                END;
                                --
                        END IF;
                        --
                        IF v_ten_conf_mill_flag = ods_common_pkg.c_str_yes OR v_portfolio_1 = ods_common_pkg.c_str_yes THEN
                            v_completeness_flag    := ods_common_pkg.c_str_yes;
                                v_broken_flag        := ods_common_pkg.c_str_no;
                        ELSE
                            --
                                v_completeness_flag    := ods_common_pkg.c_str_no;
                                v_flag := '6';

                                BEGIN
                                        query6 := 'SELECT ''N''
                                          FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
                                         WHERE 0 = 0 AND ods_busn_trx_table.doc_portfolio_type = ''02''
                                               AND (secondary_costing_flag = ''N''
                                                    OR (secondary_costing_flag = ''Y''
                                                        AND doc_portfolio_type = ''40''))
                                               --
                                               AND NOT EXISTS
                                                           (SELECT 1
                                                              FROM ods_deliveries d,
                                                                   TABLE (
                                                                       CAST (
                                                                           :v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                             WHERE d.so_commercial_location_code =
                                                                       ods_busn_trx_table_aux.doc_comm_location_code
                                                                   AND d.so_doc_type_code =
                                                                           ods_busn_trx_table_aux.doc_type_code
                                                                   AND d.so_order_year =
                                                                           ods_busn_trx_table_aux.document_year
                                                                   AND d.so_order_month =
                                                                           ods_busn_trx_table_aux.document_month
                                                                   AND d.so_number =
                                                                           ods_busn_trx_table_aux.document_number
                                                                   AND d.so_item_num =
                                                                           ods_busn_trx_table_aux.document_item_num
                                                                   AND d.so_split_num =
                                                                           ods_busn_trx_table_aux.document_split_num
                                                                   AND ods_busn_trx_table.business_id =
                                                                           ods_busn_trx_table_aux.business_id)
                                               --
                                               AND ROWNUM = 1';

                                        query6 := query6 || condicion;

                                        EXECUTE IMMEDIATE (query6)
                                        INTO v_broken_flag
                                        USING p_busn_trx_data_table, p_busn_trx_data_table;
                                --
                                EXCEPTION
                                        WHEN NO_DATA_FOUND THEN
                                                 v_broken_flag := ods_common_pkg.c_str_yes;
                                END;
                                --
                        END IF;

				END IF;

        ELSIF p_typology_code IN /*('BRC')*/  ('BSR') THEN

				BEGIN		--ITTEN00264345
                        v_flag := '7';

                        query7 := 'SELECT b.business_sequence
                          FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) b
                         WHERE 0 = 0
                               AND b.business_sequence =
                                       (SELECT MIN (a.business_sequence)
                                          FROM TABLE (
                                                   CAST (
                                                       :v_tabla AS ods_busn_trx_table_type)) a
                                         WHERE 0 = 0
                                               AND (a.business_reference_id = 0
                                                    OR a.business_reference_id IS NULL)
                                               AND a.doc_portfolio_type = ''88''';

                           query8 := ')
                                   + 1
                           AND b.doc_portfolio_type = ''01''
                           AND b.parent_doc_portfolio_type = ''06''
                           AND (b.business_reference_id = 0 OR b.business_reference_id IS NULL)';

                        query7 := query7 || condicion || query8;

                        EXECUTE IMMEDIATE (query7)
                        INTO v_business_sequence_01
                        USING p_busn_trx_data_table, p_busn_trx_data_table;
                --
                EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                            v_business_sequence_01 := 0;
                END;

                IF v_business_sequence_01 <> 0 THEN			--ITTEN00264345
                        v_ten_conf_mill_flag := ods_common_pkg.c_str_yes;
                        v_broken_flag := ods_common_pkg.c_str_no;

                        BEGIN
                                v_flag := '8';

                                query9 := 'SELECT c.business_sequence
                                  FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) c
                                 WHERE	   0 = 0
                                       AND c.business_sequence > :v_business_sequence_01
                                       AND c.doc_portfolio_type = ''88''
                                       AND (c.business_reference_id = 0 OR c.business_reference_id IS NULL)
                                       AND ROWNUM = 1';

                                query9 := query9 || condicion;

                                EXECUTE IMMEDIATE (query9)
                                INTO v_business_sequence_88
                                USING p_busn_trx_data_table, v_business_sequence_01;
                        --
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                            		v_business_sequence_88 := 0;
                                    v_completeness_flag := ods_common_pkg.c_str_no;
                        END;

                        IF v_business_sequence_88 <> 0 THEN
                                v_completeness_flag := ods_common_pkg.c_str_yes;
                        END IF;

                END IF;

                IF v_business_sequence_01 = 0 THEN

                        BEGIN
                            v_flag := '9';

                                query10 := 'SELECT ''Y'', business_reference_id
                                  FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
                                 WHERE 0 = 0 AND doc_portfolio_type = ''88''
                                       AND (secondary_costing_flag = ''N''
                                            OR (secondary_costing_flag = ''Y''
                                                AND doc_portfolio_type = ''40''))
                                       AND ROWNUM = 1';

                                query10 := query10 || condicion;

                                EXECUTE IMMEDIATE (query10)
                                INTO v_ten_conf_mill_flag, v_business_reference_id
                                USING p_busn_trx_data_table;
                        --
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                    v_ten_conf_mill_flag := ods_common_pkg.c_str_no;
                                        v_business_reference_id := NULL;
                        END;

                        IF v_ten_conf_mill_flag = ods_common_pkg.c_str_no THEN
                            --
                                BEGIN
                                        v_flag := '10';

                                        query11 := 'SELECT ''Y'', business_reference_id
                                          FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
                                         WHERE 0 = 0
                                               AND (secondary_costing_flag = ''N''
                                                    OR (secondary_costing_flag = ''Y''
                                                        AND doc_portfolio_type = ''40''))
                                               AND ODS_BUSINESS_TRANSACTION_PKG.is_supplier_not_tenaris_fn (
                                                   ods_busn_trx_table.doc_comm_location_code,
                                                   ods_busn_trx_table.document_number,
                                                   ods_busn_trx_table.document_month,
                                                   ods_busn_trx_table.document_year,
                                                   ods_busn_trx_table.doc_type_code) = ''Y''
                                               AND ROWNUM = 1';

                                        query11 := query11 || condicion;

                                        EXECUTE IMMEDIATE (query11)
                                        INTO v_portfolio_1, v_business_reference_id
                                        USING p_busn_trx_data_table;
                               --
                                EXCEPTION
                                        WHEN NO_DATA_FOUND THEN
                                                 v_portfolio_1 := ods_common_pkg.c_str_no;
                                                 v_business_reference_id := NULL;
                                END;
                                --
                        END IF;
                        --
                        IF v_ten_conf_mill_flag = ods_common_pkg.c_str_yes OR v_portfolio_1 = ods_common_pkg.c_str_yes THEN
                            v_completeness_flag    := ods_common_pkg.c_str_yes;
                                v_broken_flag        := ods_common_pkg.c_str_no;
                        ELSE
                            --
                                v_completeness_flag    := ods_common_pkg.c_str_no;
                                v_flag := '11';

                                BEGIN
                                        query12 := 'SELECT ''Y''
                                          FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
                                         WHERE 0 = 0
                                               AND (secondary_costing_flag = ''N''
                                                    OR (secondary_costing_flag = ''Y''
                                                        AND doc_portfolio_type = ''40''))
                                               AND EXISTS
                                                       (SELECT 1
                                                          FROM TABLE (
                                                                   CAST (
                                                                       :v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table_aux
                                                         WHERE 0 = 0
                                                               AND ods_busn_trx_table_aux.business_id =
                                                                       ods_busn_trx_table.business_id
                                                               AND ods_busn_trx_table_aux.doc_portfolio_type = ''02''
                                                               AND ods_busn_trx_table_aux.business_sequence = 1
                                                               AND (ods_busn_trx_table_aux.secondary_costing_flag =
                                                                        ''N''
                                                                    OR (ods_busn_trx_table_aux.secondary_costing_flag =
                                                                            ''Y''
                                                                        AND ods_busn_trx_table_aux.doc_portfolio_type =
                                                                                ''40''))
                                                               AND ods_busn_trx_table_aux.historical_link_flag =
                                                                       ''N''
                                                               AND ODS_BUSINESS_TRANSACTION_PKG.exist_stock_fn (
                                                                       ods_busn_trx_table_aux.doc_comm_location_code,
                                                                       ods_busn_trx_table_aux.document_number,
                                                                       ods_busn_trx_table_aux.document_month,
                                                                       ods_busn_trx_table_aux.document_year,
                                                                       ods_busn_trx_table_aux.doc_type_code,
                                                                       ods_busn_trx_table_aux.document_item_num,
                                                                       ods_busn_trx_table_aux.document_split_num,
                                                                       ods_busn_trx_table_aux.doc_portfolio_type) =
                                                                       ''N'')
                                               AND ROWNUM = 1';

                                        query12 := query12 || condicion;

                                        EXECUTE IMMEDIATE (query12)
                                        INTO v_broken_flag
                                        USING p_busn_trx_data_table, p_busn_trx_data_table;
                                --
                                EXCEPTION
                                        WHEN NO_DATA_FOUND THEN
                                                 v_broken_flag := ods_common_pkg.c_str_no;

                                END;
                                --
                        END IF;

        		END IF;

        ELSIF p_typology_code IN ('SRV') THEN
            	--
        		v_broken_flag := ods_common_pkg.c_str_no;
        		v_completeness_flag := ods_common_pkg.c_str_yes;
                --
                BEGIN
                    v_flag := '12';

                        query13 := 'SELECT business_reference_id
                          FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
                         WHERE 0 = 0
                               AND (secondary_costing_flag = ''N''
                                    OR (secondary_costing_flag = ''Y''
                                        AND doc_portfolio_type = ''40''))
                               AND ROWNUM = 1';

                        query13 := query13 || condicion;

                        EXECUTE IMMEDIATE (query13)
                        INTO v_business_reference_id
                        USING p_busn_trx_data_table;
                END;
                --
        ELSIF p_typology_code = '999' THEN
            --
                v_flag := '13';
                v_broken_flag := ods_common_pkg.c_str_yes;
                v_completeness_flag := ods_common_pkg.c_str_no;
                v_ten_conf_mill_flag := '9';
                v_business_reference_id := NULL;
                --
        END IF;
        --
        p_completeness_flag := v_completeness_flag;
        p_broken_flag := v_broken_flag;
        p_ten_conf_mill_flag := v_ten_conf_mill_flag;
        p_business_ref_id := v_business_reference_id;
        --

EXCEPTION
		WHEN OTHERS
        THEN
        v_errm := SUBSTR(SQLERRM, 1 , 64);
		ODS_ERROR_LOG_PKG.INSERT_ERROR_LOG_TABLE ('CALC_STUTUS_CLASS_SP'||' - '||v_flag
                                                , v_errm
                                                , NULL
                                                , 'TENARIS'
                                                , SYSDATE
                                                , 'TENARIS');
         RAISE;

END calc_stutus_class_sp;

--------------------------------------------------------------------------------

PROCEDURE calc_finishing_order_sp (
                p_busn_trx_data_table           IN ods_busn_trx_table_type,
                p_typology_code                 IN ods_busn_tree_classifications.business_typology_code%TYPE,
                p_historical_chain_flag         IN ods_busn_tree_classifications.historical_chain_flag%TYPE,
                p_finishing_order_key           OUT ods_busn_tree_classifications.finishing_order_key%TYPE,
                p_finishing_process_code        OUT ods_busn_tree_classifications.finishing_process_classn_code%TYPE,
                p_finishing_flag                OUT ods_busn_tree_classifications.finishing_reference_flag%TYPE
                )
IS
        v_comm_loc              ods_business_transactions.doc_comm_location_code%TYPE;
        v_doc_year              ods_business_transactions.document_year%TYPE;
        v_doc_month             ods_business_transactions.document_month%TYPE;
        v_doc_type              ods_business_transactions.doc_type_code%TYPE;
        v_doc_num               ods_business_transactions.document_number%TYPE;
        v_doc_itm_num           ods_business_transactions.document_item_num%TYPE;
        v_doc_spl_num           ods_business_transactions.document_split_num%TYPE;
        v_doc_portfolio         ods_business_transactions.doc_portfolio_type%TYPE;
        v_exist_purchase        ods_common_pkg.s_str_yes_no;
        --
        v_sql_hist_cond         VARCHAR2(30);
        v_sql_min_seq_hist_cond VARCHAR2(65);
        v_sql                   VARCHAR2(1300);
        v_sql_min_seq           VARCHAR2(600);
--
BEGIN
        --Determinamos si consideramos el historical_link_flag como parte de los filtros del query que determinará el Finishing Order
        IF p_historical_chain_flag = ods_common_pkg.c_str_no THEN
                --
                v_sql_hist_cond := 'historical_link_flag = ''N'' ';
                v_sql_min_seq_hist_cond := ' ods_busn_trx_table_aux.' || v_sql_hist_cond;
                --
        ELSE
                --
                v_sql_hist_cond := ' ''N'' = ''N'' ';
                v_sql_min_seq_hist_cond := v_sql_hist_cond;
                --
        END IF;

        --Cargamos la variable con el subquery que determina la menor secuencia en una cadena de documentos
        v_sql_min_seq := '(SELECT MIN (ods_busn_trx_table_aux.business_sequence) ' ||
                         'FROM   TABLE (CAST (:p_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table_aux ' ||
                         'WHERE  0=0 '||
                         'AND    ods_busn_trx_table_aux.business_id = ods_busn_trx_table.business_id ' ||
                         'AND    ods_busn_trx_table_aux.doc_portfolio_type IN (''04'',''88'') ' ||
                         'AND    (   ods_busn_trx_table_aux.business_reference_id = 0 ' ||
                         '        OR ods_busn_trx_table_aux.business_reference_id IS NULL)' ||
                         'AND    ' || v_sql_min_seq_hist_cond ||
                         ') ';

        --Cargamos la variable con la base del query que determina la Finishing Order de la clasificacion de un documento
        v_sql := 'SELECT  ods_busn_trx_table.doc_comm_location_code, '||
                 '        ods_busn_trx_table.document_year, '||
                 '        ods_busn_trx_table.document_month, '||
                 '        ods_busn_trx_table.doc_type_code, '||
                 '        ods_busn_trx_table.document_number, '||
                 '        ods_busn_trx_table.document_item_num, '||
                 '        ods_busn_trx_table.document_split_num, '||
                 '        ods_busn_trx_table.doc_portfolio_type '||
                 'FROM    TABLE (CAST (:p_busn_trx_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table '||
                 'WHERE   0=0 '||
                 'AND     doc_portfolio_type IN (''04'',''88'') '||
                 'AND     (   ods_busn_trx_table.business_reference_id = 0 '||
                 '         OR ods_busn_trx_table.business_reference_id IS NULL '||
                 '        )'||
                 'AND     business_sequence = ' || v_sql_min_seq ||
                 'AND    ' || v_sql_hist_cond ||
                 'AND     ROWNUM = 1';
        --
        --Ejecutamos el query dinamico para obtener los parametros de la Finishing Order
        BEGIN
                EXECUTE IMMEDIATE (v_sql)
                INTO v_comm_loc, v_doc_year, v_doc_month, v_doc_type, v_doc_num, v_doc_itm_num, v_doc_spl_num, v_doc_portfolio
                USING p_busn_trx_data_table, p_busn_trx_data_table;
        EXCEPTION
                WHEN no_data_found THEN
                        v_comm_loc := NULL;
                        v_doc_year := NULL;
                        v_doc_month := NULL;
                        v_doc_type := NULL;
                        v_doc_num := NULL;
                        v_doc_itm_num := NULL;
                        v_doc_spl_num := NULL;
                        v_doc_portfolio := NULL;
        END;
        --
        --Determinamos la Finishing Order Key, el Finishing Flag y Finishing Process Code
        IF v_doc_num IS NOT NULL AND v_doc_portfolio = '04' THEN
                --
                p_finishing_order_key := v_comm_loc || '-' || LPAD(v_doc_year,4,0) || '-' || LPAD(v_doc_month,2,0) || '-' || RPAD(v_doc_type,2,' ') || '-' || LPAD(v_doc_num,8,0) || '-' || LPAD(v_doc_itm_num,5,0) || '-' || LPAD(v_doc_spl_num,5,0) || '-' || RPAD(v_doc_portfolio,2,' ');
                --
                p_finishing_flag := ods_common_pkg.c_str_no;
                --
                --Cargamos el query dinamico para determinar si existe una orden de compra en la cadena a procesar
                v_sql := 'SELECT ''' || ods_common_pkg.c_str_yes || ''', ' ||
                         '        ods_busn_trx_table.doc_comm_location_code, '||
                         '        ods_busn_trx_table.document_year, '||
                         '        ods_busn_trx_table.document_month, '||
                         '        ods_busn_trx_table.doc_type_code, '||
                         '        ods_busn_trx_table.document_number '||
                         'FROM	 TABLE(CAST(:v_data_table AS ods_busn_trx_table_type)) ods_busn_trx_table '||
                         'WHERE	 0=0 '||
                         'AND 	 doc_portfolio_type = ''40'' '||
                         'AND	 (	secondary_costing_flag = ''' || ods_common_pkg.c_str_no || '''' ||
                         '	  OR    (secondary_costing_flag = ''' || ods_common_pkg.c_str_yes || ''' AND doc_portfolio_type = ''40'') ' ||
                         '	 ) '||
                         'AND    ROWNUM = 1 '||
                         'AND    ' || v_sql_hist_cond;
                --
                --Ejecutamos el query dinamico
                BEGIN
                        EXECUTE IMMEDIATE (v_sql)
                        INTO v_exist_purchase, v_comm_loc, v_doc_year, v_doc_month, v_doc_type, v_doc_num
                        USING p_busn_trx_data_table;
                EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                                v_exist_purchase := ods_common_pkg.c_str_no;
                                v_comm_loc := NULL;
                                v_doc_year := NULL;
                                v_doc_month := NULL;
                                v_doc_type := NULL;
                                v_doc_num := NULL;
                END;
                --
                --Determinamos el Finishing Process Code para la Finishing Order obtenida si es que esta tiene asociada una ordern de compra
                IF v_exist_purchase = ods_common_pkg.c_str_yes THEN
                        --
                        --Verificamos si la orden de compra obtenida es un cliente no Tenaris
                        IF is_supplier_not_tenaris_fn(v_comm_loc,v_doc_num,v_doc_month,v_doc_year,v_doc_type) = ods_common_pkg.c_str_yes THEN
                                --
                                IF p_typology_code = 'SRV' THEN
                                        --
                                        p_finishing_process_code := '03';
                                        --
                                ELSE
                                        --
                                        p_finishing_process_code := '01';
                                        --
                                END IF;
                                --
                        ELSE
                                --
                                IF p_typology_code = 'SRV' THEN
                                        --
                                        p_finishing_process_code := '02';
                                        --
                                ELSE
                                        --
                                        p_finishing_process_code := '04';
                                        --
                                END IF;
                                --
                        END IF;
                        --
                ELSIF v_exist_purchase = ods_common_pkg.c_str_no THEN
                        --
                        IF p_typology_code = 'SRV' THEN
                                --
                                p_finishing_process_code := '02';
                                --
                        ELSE
                                --
                                p_finishing_process_code := '04';
                                --
                        END IF;
                        --
                END IF;
                --
        ELSIF v_doc_num IS NOT NULL AND v_doc_portfolio = '88' THEN
                --
                p_finishing_order_key := v_comm_loc || '-' || LPAD(v_doc_year,4,0) || '-' || LPAD(v_doc_month,2,0) || '-' || RPAD(v_doc_type,2,' ') || '-' || LPAD(v_doc_num,8,0) || '-' || LPAD(v_doc_itm_num,5,0) || '-' || LPAD(v_doc_spl_num,5,0) || '-' || RPAD(v_doc_portfolio,2,' ');
                p_finishing_process_code := '04';
                p_finishing_flag := ods_common_pkg.c_str_no;
                --
        ELSE
                --
                p_finishing_order_key := NULL;
                p_finishing_process_code := '05';
                p_finishing_flag := NULL;
                --
        END IF;
        --
END calc_finishing_order_sp;

PROCEDURE calc_finishing_plant_sp
        (p_busn_trx_data_table  IN ods_busn_trx_table_type,
         p_finishing_order_key  IN ods_busn_tree_classifications.finishing_order_key%TYPE,
         p_finishing_plant      OUT ods_busn_tree_classifications.finishing_plant_code%TYPE)
IS
        v_business_id           ods_business_transactions.business_id%TYPE;
        v_plant_code		ods_bt_service_sales.SS_PLANT_CODE%TYPE;
        v_plant_code_not_inf    ods_bt_service_sales.SS_PLANT_CODE%TYPE;

    	v_comm_loc        	ods_business_transactions.doc_comm_location_code%TYPE;
        v_doc_year        	ods_business_transactions.document_year%TYPE;
        v_doc_month        	ods_business_transactions.document_month%TYPE;
        v_doc_type        	ods_business_transactions.doc_type_code%TYPE;
        v_doc_num        	ods_business_transactions.document_number%TYPE;
        v_doc_itm_num           ods_business_transactions.document_item_num%TYPE;
        v_doc_spl_num           ods_business_transactions.document_split_num%TYPE;
        v_doc_portfolio         ods_business_transactions.doc_portfolio_type%TYPE;
        v_bus_ref_id            ods_business_transactions.business_reference_id%TYPE;
        v_exist_purchase        ods_common_pkg.s_str_yes_no;
        v_flag        		NUMBER (1) := 0;
        v_count_doc		NUMBER;
        --
BEGIN

        BEGIN
--                query1 := 'SELECT ods_busn_trx_table.business_id
--                             FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
--			                WHERE doc_portfolio_type = ''04''
--		                      AND ROWNUM = 1
--                         ORDER BY ods_busn_trx_table.business_sequence';

--                --query1 := query1 || condicion;

--                EXECUTE IMMEDIATE (query1)
--                INTO v_business_id
--                USING p_busn_trx_data_table;

                --Buscamos en la cadena una orden de Finishing
                select  ods_busn_trx_table.business_id
                into    v_business_id
                from    table (cast (p_busn_trx_data_table as ods_busn_trx_table_type)) ods_busn_trx_table
                where   doc_portfolio_type = '04'
                and     rownum = 1
                order by ods_busn_trx_table.business_sequence;

		begin

                        --Buscamos en la cadena, una Orden de Venta
                        select  1
                        into    v_count_doc
                        from    table (cast (p_busn_trx_data_table as ods_busn_trx_table_type)) ods_busn_trx_table
                        where   doc_portfolio_type = '02'
                        and     rownum = 1
                        order by ods_busn_trx_table.business_sequence;

                        --Buscamos la planta en la orden de Venta, para verificar que el negocio
                        --tiene el modelo de Hydril
	                select  btss.ss_plant_code
                        into    v_plant_code
                        from    ods_bt_service_sales btss
                        where   btss.business_id = v_business_id;

		exception
                	when no_data_found then
                		--Para el resto de las Sociedades, la planta de roscado se encuentra
                                --en la Orden de Producción
                                BEGIN
                                        SELECT  oitm.PLANT_CODE
                                        INTO    v_plant_code
                                        FROM    adas.ods_production_orders_itm oitm
                                        WHERE   oitm.PRO_NUMBER = SUBSTR(p_finishing_order_key,16,8)
                                        AND     oitm.PRO_ITEM_NUM = TO_NUMBER(SUBSTR(p_finishing_order_key,25,5))
                                        AND     oitm.DOC_TYPE_CODE = SUBSTR(p_finishing_order_key,13,2)
                                        AND     oitm.COMMERCIAL_LOCATION_CODE = SUBSTR(p_finishing_order_key,1,3)
                                        AND     oitm.PRO_ORDER_YEAR = SUBSTR(p_finishing_order_key,5,4)
                                        AND     oitm.PRO_ORDER_MONTH = SUBSTR(p_finishing_order_key,10,2);

				EXCEPTION
                		        WHEN no_data_found THEN
                                	        v_plant_code := NULL;
				END;
				--
                end;
		--
        EXCEPTION
                WHEN NO_DATA_FOUND THEN
                        --No existe un documento de Venta
                        v_plant_code := NULL;

        END;

	--dbms_output.put_line('p_finishing_plant: '||v_plant_code);

        if v_plant_code is null then

                begin
                        select pl.PLANT_CODE
                        into v_plant_code
                        from ods_plants pl
                        where pl.PLANT_DESC = 'NOT INFORMED';

                EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                                --No existe el código NOT INFORMED
                                v_plant_code := NULL;
		end;

        end if;

        p_finishing_plant := v_plant_code;

        --dbms_output.put_line('calc_finishing_plant_sp - Fin');

end calc_finishing_plant_sp;
--------------------------------------------------------------------------------

PROCEDURE calc_conforming_order_sp (p_busn_trx_data_table     IN    ods_busn_trx_table_type,
                    p_completeness_flag        IN    ods_busn_tree_classifications.completeness_flag%TYPE,
					p_historical_chain_flag    IN	  ods_busn_tree_classifications.historical_chain_flag%TYPE,
                    p_conforming_order_key    OUT    ods_busn_tree_classifications.finishing_order_key%TYPE,
                   p_conforming_flag OUT    ods_busn_tree_classifications.conforming_reference_flag%TYPE
                                   )
IS
    v_comm_loc        ods_business_transactions.doc_comm_location_code%TYPE;
        v_doc_year        ods_business_transactions.document_year%TYPE;
        v_doc_month        ods_business_transactions.document_month%TYPE;
        v_doc_type        ods_business_transactions.doc_type_code%TYPE;
        v_doc_num        ods_business_transactions.document_number%TYPE;
        v_doc_itm_num        ods_business_transactions.document_item_num%TYPE;
        v_doc_spl_num        ods_business_transactions.document_split_num%TYPE;
        v_doc_portfolio        ods_business_transactions.doc_portfolio_type%TYPE;
        v_bus_ref_id        ods_business_transactions.business_reference_id%TYPE;
        v_exist            ods_common_pkg.s_str_yes_no;
        v_flag        number (1) := 0;
        query1  varchar2 (4000);
        query2  varchar2 (4000);
        query3  varchar2 (4000);
        condicion varchar2 (100);
--
BEGIN
    --
        IF p_historical_chain_flag = 'Y' THEN
                condicion := ' AND ''N'' = ''N''';
        ELSE
                condicion := ' AND historical_link_flag = ''N''';
        END IF;
		--
        IF p_completeness_flag = ods_common_pkg.c_str_yes THEN
            --
                BEGIN
                        query1 := 'SELECT ods_busn_trx_table.doc_comm_location_code,
                               ods_busn_trx_table.document_year,
                               ods_busn_trx_table.document_month,
                               ods_busn_trx_table.doc_type_code,
                               ods_busn_trx_table.document_number,
                               ods_busn_trx_table.document_item_num,
                               ods_busn_trx_table.document_split_num,
                               ods_busn_trx_table.doc_portfolio_type,
                               ods_busn_trx_table.business_reference_id,
                               1
                          FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
                         WHERE 0 = 0 AND doc_portfolio_type = ''88''
                               AND (secondary_costing_flag = ''N''
                                    OR (secondary_costing_flag = ''Y''
                                        AND doc_portfolio_type = ''40''))
                               AND business_sequence =
                                       (SELECT MAX (ods_busn_trx_table_aux2.business_sequence)
                                          FROM TABLE (
                                                   CAST (
                                                       :v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table_aux2
                                         WHERE 0 = 0
                                               AND ods_busn_trx_table_aux2.business_id =
                                                       ods_busn_trx_table.business_id
                                               AND ods_busn_trx_table_aux2.doc_portfolio_type =
                                                       ods_busn_trx_table.doc_portfolio_type
                                               AND (ods_busn_trx_table_aux2.secondary_costing_flag =
                                                        ''N''
                                                    OR (ods_busn_trx_table_aux2.
                                                         secondary_costing_flag =
                                                            ''Y''
                                                        AND ods_busn_trx_table_aux2.
                                                             doc_portfolio_type = ''40''))
                                               AND ods_busn_trx_table_aux2.historical_link_flag =
                                                       ''N''
                                               AND (ods_busn_trx_table_aux2.business_reference_id = 0
                                                    OR ods_busn_trx_table_aux2.business_reference_id
                                                           IS NULL))
                               AND ROWNUM = 1';

                        query1 := query1 || condicion;

                        EXECUTE IMMEDIATE (query1)
                        INTO v_comm_loc, v_doc_year, v_doc_month, v_doc_type, v_doc_num, v_doc_itm_num, v_doc_spl_num, v_doc_portfolio, v_bus_ref_id, v_flag
                        USING p_busn_trx_data_table, p_busn_trx_data_table;
                --
                EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                                 v_comm_loc := NULL;
                                 v_doc_year := NULL;
                                 v_doc_month := NULL;
                                 v_doc_type := NULL;
                                 v_doc_num := NULL;
                                 v_doc_itm_num := NULL;
                                 v_doc_spl_num := NULL;
                                 v_doc_portfolio := NULL;
                                 v_bus_ref_id := NULL;
                                 v_flag :=0;
                END;

                IF (v_flag = 0) THEN

                        BEGIN
                                query2 := 'SELECT ods_busn_trx_table.doc_comm_location_code,
                                       ods_busn_trx_table.document_year,
                                       ods_busn_trx_table.document_month,
                                       ods_busn_trx_table.doc_type_code,
                                       ods_busn_trx_table.document_number,
                                       ods_busn_trx_table.document_item_num,
                                       ods_busn_trx_table.document_split_num,
                                       ods_busn_trx_table.doc_portfolio_type,
                                       ods_busn_trx_table.business_reference_id,
                                       0
                                  FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
                                 WHERE 0 = 0 AND doc_portfolio_type = ''88''
                                       AND (secondary_costing_flag = ''N''
                                            OR (secondary_costing_flag = ''Y''
                                                AND doc_portfolio_type = ''40''))
                                       AND business_sequence =
                                               (SELECT MAX (ods_busn_trx_table_aux2.business_sequence)
                                                  FROM TABLE (
                                                           CAST (
                                                               :v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table_aux2
                                                 WHERE 0 = 0
                                                       AND ods_busn_trx_table_aux2.business_id =
                                                               ods_busn_trx_table.business_id
                                                       AND ods_busn_trx_table_aux2.doc_portfolio_type =
                                                               ods_busn_trx_table.doc_portfolio_type
                                                       AND (ods_busn_trx_table_aux2.secondary_costing_flag =
                                                                ''N''
                                                            OR (ods_busn_trx_table_aux2.
                                                                 secondary_costing_flag =
                                                                    ''Y''
                                                                AND ods_busn_trx_table_aux2.
                                                                     doc_portfolio_type = ''40''))
                                                       AND ods_busn_trx_table_aux2.historical_link_flag =
                                                               ''N'')
                                       AND ROWNUM = 1';

                                query2 := query2 || condicion;

                                EXECUTE IMMEDIATE (query2)
                                INTO v_comm_loc, v_doc_year, v_doc_month, v_doc_type, v_doc_num, v_doc_itm_num, v_doc_spl_num, v_doc_portfolio, v_bus_ref_id, v_flag
                                USING p_busn_trx_data_table, p_busn_trx_data_table;
						--
                        EXCEPTION
                                WHEN NO_DATA_FOUND THEN
                                        BEGIN
                                                query3 := 'SELECT ods_busn_trx_table.doc_comm_location_code,
                                                       ods_busn_trx_table.document_year,
                                                       ods_busn_trx_table.document_month,
                                                       ods_busn_trx_table.doc_type_code,
                                                       ods_busn_trx_table.document_number,
                                                       ods_busn_trx_table.document_item_num,
                                                       ods_busn_trx_table.document_split_num,
                                                       ods_busn_trx_table.doc_portfolio_type,
                                                       1
                                                  FROM TABLE (CAST (:v_tabla AS ods_busn_trx_table_type)) ods_busn_trx_table
                                                 WHERE 0 = 0
                                                       AND (secondary_costing_flag = ''N''
                                                            OR (secondary_costing_flag = ''Y''
                                                                AND doc_portfolio_type = ''40''))
                                                       AND ODS_BUSINESS_TRANSACTION_PKG.is_supplier_not_tenaris_fn (
                                                               ods_busn_trx_table.doc_comm_location_code,
                                                               ods_busn_trx_table.document_number,
                                                               ods_busn_trx_table.document_month,
                                                               ods_busn_trx_table.document_year,
                                                               ods_busn_trx_table.doc_type_code) = ''Y''
                                                       AND ROWNUM = 1';

                                                query3 := query3 || condicion;

                                                EXECUTE IMMEDIATE (query3)
                                                INTO v_comm_loc, v_doc_year, v_doc_month, v_doc_type, v_doc_num, v_doc_itm_num, v_doc_spl_num, v_doc_portfolio, v_flag
                                                USING p_busn_trx_data_table;
                                        --
                                        EXCEPTION
                                                WHEN NO_DATA_FOUND THEN
                                                        p_conforming_order_key := NULL;
                                                        v_comm_loc := NULL;
                                                        v_doc_year := NULL;
                                                        v_doc_month := NULL;
                                                        v_doc_type := NULL;
                                                        v_doc_num := NULL;
                                                        v_doc_itm_num := NULL;
                                                        v_doc_spl_num := NULL;
                                                        v_doc_portfolio := NULL;
                                                        v_bus_ref_id := NULL;
                                                        v_flag := 1;
                                        END;

                        END;

                END IF;
                --
                IF v_doc_num IS NOT NULL THEN
                    --
                    p_conforming_order_key := v_comm_loc || '-' || LPAD(v_doc_year,4,0) || '-' || LPAD(v_doc_month,2,0) || '-' || RPAD(v_doc_type,2,' ') || '-' || LPAD(v_doc_num,8,0) || '-' || LPAD(v_doc_itm_num,5,0) || '-' || LPAD(v_doc_spl_num,5,0) || '-' || RPAD(v_doc_portfolio,2,' ');
                    --
                END IF;

                IF (v_flag = 1) THEN
                        p_conforming_flag := ods_common_pkg.c_str_no;
                ELSE
                        p_conforming_flag := ods_common_pkg.c_str_yes;
                END IF;
                --
        END IF;
        --
END calc_conforming_order_sp;

--------------------------------------------------------------------------------

PROCEDURE update_bsn_tree_classn_sp(
        p_business_id        IN ods_busn_tree_classifications.business_id%TYPE,
        p_broken_flag        IN ods_busn_tree_classifications.broken_flag%TYPE,
        p_last_updated_date    IN ods_busn_tree_classifications.last_updated_date%TYPE,
        p_last_updated_by    IN ods_busn_tree_classifications.last_updated_by%TYPE
        )
IS
BEGIN
    IF (p_broken_flag IS NULL
    AND	v_g_completeness_flag = ods_common_pkg.c_str_yes)
    THEN
			BEGIN
            --Actualizamos la tabla ODS_BUSINESS_DOC_TREE_WRK con el BId que se Completo
				 MERGE INTO ods_business_doc_tree_wrk wrk
     			      USING (SELECT	business_id
            	 	         FROM	ods_busn_tree_classifications
              		   		 WHERE	business_id = p_business_id) BTC
  	                	ON (WRK.business_id = BTC.business_id)
				 WHEN MATCHED THEN
  	   			 UPDATE SET	process_status = 'A'
			    			,created_date = SYSDATE
							,created_by ='ONE'
							,last_updated_date = SYSDATE
							,last_updated_by ='ONE'
				 WHEN NOT MATCHED THEN
				 INSERT	(wrk.business_id
                		,wrk.process_status
                		,wrk.created_date
                		,wrk.created_by
                		,wrk.last_updated_date
                		,last_updated_by)
				 VALUES	(btc.business_id
						,'A'
			    		,SYSDATE
						,'ONE'
						,SYSDATE
						,'ONE');
            --
                UPDATE    ods_busn_tree_classifications
                SET    last_updated_date = p_last_updated_date,
                        last_updated_by = p_last_updated_by
                WHERE    0=0
                AND    business_id = p_business_id;
			--
			END;
    ELSIF p_broken_flag = ods_common_pkg.c_str_yes
    THEN
            BEGIN
            --Actualizamos la tabla ODS_BUSINESS_DOC_TREE_WRK con el BId de la cadena rota
				 MERGE INTO ods_business_doc_tree_wrk wrk
     			      USING (SELECT	business_id
            	 	         FROM	ods_busn_tree_classifications
              		   		 WHERE	business_id = p_business_id) BTC
  	                	ON (WRK.business_id = BTC.business_id)
				 WHEN MATCHED THEN
  	   			 UPDATE SET	process_status = 'A'
			    			,created_date = SYSDATE
							,created_by ='ONE'
							,last_updated_date = SYSDATE
							,last_updated_by ='ONE'
				 WHEN NOT MATCHED THEN
				 INSERT	(wrk.business_id
                		,wrk.process_status
                		,wrk.created_date
                		,wrk.created_by
                		,wrk.last_updated_date
                		,last_updated_by)
				 VALUES	(btc.business_id
						,'A'
			    		,SYSDATE
						,'ONE'
						,SYSDATE
						,'ONE');

             --Se marca la cadena con el BROKEN_FLAG = 'Y', para no evaluarla nuevamente
             	 UPDATE    ods_busn_tree_classifications
                 SET    broken_flag = p_broken_flag,
                 		last_updated_date = p_last_updated_date,
                        last_updated_by = p_last_updated_by
                 WHERE  0=0
                 AND    business_id = p_business_id;
            END;
    END IF;
        --
        COMMIT;
        --
END update_bsn_tree_classn_sp;

--------------------------------------------------------------------------------

PROCEDURE reclassify_list_busn_trx_sp
IS
    CURSOR cur_busn_tree_classn
        IS
        SELECT    business_id,
            created_date,
            created_by
        FROM    ods_busn_tree_classifications btc
        WHERE    0=0
        AND    btc.completeness_flag = 'N'
        AND    btc.broken_flag = 'N' ;

        v_busn_tree_classn_rec cur_busn_tree_classn%ROWTYPE;
BEGIN
    --
    FOR v_busn_tree_classn_rec IN cur_busn_tree_classn
        LOOP
            --
            --Seteamos la variable interna con el estado de completitud del Business.
            --Si el mismo se Completa dentro del classify_business_trx_sp, le asignamos 'Y'.
            v_g_completeness_flag := ods_common_pkg.c_str_no;
            --
                IF v_busn_tree_classn_rec.created_date > SYSDATE - 10 THEN
                        --
                        classify_business_trx_sp(
                                p_business_id        => v_busn_tree_classn_rec.business_id,
                                p_data_source_system    => v_busn_tree_classn_rec.created_by
                                );
                        --
                        update_bsn_tree_classn_sp(
                            p_business_id        => v_busn_tree_classn_rec.business_id,
                                p_broken_flag        => NULL,
                                p_last_updated_date    => SYSDATE,
                                p_last_updated_by    => c_busn_tree_rep_classn
                                );
                        --
                ELSE
                    --
                        update_bsn_tree_classn_sp(
                            p_business_id        => v_busn_tree_classn_rec.business_id,
                                p_broken_flag        => ods_common_pkg.c_str_yes,
                                p_last_updated_date    => SYSDATE,
                                p_last_updated_by    => c_busn_tree_rep_classn
                                );
                        --

                END IF;
                --
        END LOOP;
        --
END reclassify_list_busn_trx_sp;
--------------------------------------------------------------------------------

/*PROCEDURE completeness_family_product_sp(
        p_range_in_days IN PLS_INTEGER DEFAULT NULL
        )
IS
BEGIN
        UPDATE  adas.ods_business_transactions
        SET     document_family_code = ods_busn_trx_utils_pkg.get_family_id_fn(doc_comm_location_code,
                                                                                document_year,
                                                                                document_month,
                                                                                doc_type_code,
                                                                                document_number,
                                                                                document_item_num,
                                                                                doc_portfolio_type)
        WHERE 1=1
        AND   document_family_code IS NULL
        AND   (
               ( created_date > TRUNC(SYSDATE - p_range_in_days)  AND  p_range_in_days IS NOT NULL)
              OR p_range_in_days IS NULL
              );

        COMMIT;

END completeness_family_product_sp;*/
/*PROCEDURE completeness_family_product_sp( ---- MODIFICACION ITTEN00371400
        p_range_in_days IN PLS_INTEGER DEFAULT NULL
        )
IS
        CURSOR cur_get_family(
                p_c_range_in_days IN PLS_INTEGER
                )
        IS
        SELECT
                BT.BUSINESS_ID
               ,BT.BUSINESS_SEQUENCE
               ,ODS_BUSN_TRX_UTILS_PKG.GET_FAMILY_ID_FN(BT.BUSINESS_ID,BT.BUSINESS_SEQUENCE) AS DOCUMENT_FAMILY_CODE
        FROM   ODS_BUSINESS_TRANSACTIONS BT
        WHERE 1=1
        AND   BT.DOCUMENT_FAMILY_CODE IS NULL
        AND   (
              p_c_range_in_days IS NULL
              OR
              BT.CREATED_DATE > TRUNC(SYSDATE - p_c_range_in_days)
              );

        v_get_family_rec         cur_get_family%ROWTYPE;
        v_count                  PLS_INTEGER;
BEGIN
         v_count:= 0;

         FOR v_get_family_rec IN cur_get_family (
                                        p_c_range_in_days  => p_range_in_days
                                        )
         LOOP
             IF (v_get_family_rec.DOCUMENT_FAMILY_CODE IS NOT NULL) THEN
                 UPDATE  ODS_BUSINESS_TRANSACTIONS
                 SET     DOCUMENT_FAMILY_CODE    = v_get_family_rec.DOCUMENT_FAMILY_CODE
                        ,MAIN_BOUGH_BY_TYPE_FLAG = NULL
                 WHERE 1=1
                 AND   BUSINESS_ID       = v_get_family_rec.BUSINESS_ID
                 AND   BUSINESS_SEQUENCE = v_get_family_rec.BUSINESS_SEQUENCE;

                 v_count:= v_count + 1;
                 IF v_count = 2000 THEN
                    COMMIT;
                    v_count:=0;
                 END IF;

             END IF;
         END LOOP;
         COMMIT;
END completeness_family_product_sp;*/
PROCEDURE completeness_family_product_sp( ---- MODIFICACION ITTEN00371400
        p_range_in_days IN PLS_INTEGER DEFAULT NULL
        )
IS
        CURSOR cur_get_family
        IS
        SELECT BT1.BUSINESS_ID
              ,BT1.BUSINESS_SEQUENCE
              ,BT1.DOCUMENT_FAMILY_CODE
        FROM(

             ----- Production portfolio_type '04','06','70'
                     SELECT BT.BUSINESS_ID
                           ,BT.BUSINESS_SEQUENCE
                           ,P.C_FAMILY_ID AS DOCUMENT_FAMILY_CODE
                           ,BT.CREATED_DATE
                     FROM   ODS_BUSINESS_TRANSACTIONS BT
                           ,ODS_PRODUCTION_ORDERS_ITM PRO --Vista ODS_PRODUCTION_ORDERS_ITM_VW PRO
                           ,ODS_PRODUCTS P
                    WHERE 1=1
                    AND   BT.DOC_PORTFOLIO_TYPE        IN ('04','06','70')
                    AND   BT.DOCUMENT_FAMILY_CODE      IS NULL
                    AND   PRO.PRODUCT_CODE             = P.C_PRODUCT_ID
                    AND   PRO.COMMERCIAL_LOCATION_CODE = BT.DOC_COMM_LOCATION_CODE
                    AND   PRO.PRO_ORDER_YEAR           = BT.DOCUMENT_YEAR
                    AND   PRO.PRO_ORDER_MONTH          = BT.DOCUMENT_MONTH
                    AND   TRIM (PRO.DOC_TYPE_CODE)     = TRIM (BT.DOC_TYPE_CODE)
                    AND   PRO.PRO_NUMBER               = BT.DOCUMENT_NUMBER
                    AND   PRO.PRO_ITEM_NUM             = BT.DOCUMENT_ITEM_NUM

                    UNION ALL
                    ----- Production 88 portfolio_type '88'
                    SELECT BT.BUSINESS_ID
                          ,BT.BUSINESS_SEQUENCE
                          ,P.C_FAMILY_ID AS DOCUMENT_FAMILY_CODE
                          ,BT.CREATED_DATE
                    FROM   ODS_BUSINESS_TRANSACTIONS BT
                          ,ODS_PRODUCTION_ORDERS_ITM PRO --Vista ODS_PRODUCTION_ORDERS_ITM_VW PRO
                          ,ODS_PRODUCTS P
                    WHERE 1=1
                    AND   BT.DOC_PORTFOLIO_TYPE        IN ('88')
                    AND   BT.DOCUMENT_FAMILY_CODE      IS NULL
                    AND   PRO.PRODUCT_CODE             = P.C_PRODUCT_ID
                    AND   PRO.COMMERCIAL_LOCATION_CODE = BT.DOC_COMM_LOCATION_CODE
                    AND   PRO.PRO_ITEM_YEAR            = BT.DOCUMENT_YEAR
                    AND   PRO.PRO_ITEM_MONTH           = BT.DOCUMENT_MONTH
                    AND   TRIM (PRO.DOC_TYPE_CODE)     = TRIM (BT.DOC_TYPE_CODE)
                    AND   PRO.PRO_NUMBER               = BT.DOCUMENT_NUMBER
                    AND   PRO.PRO_ITEM_NUM             = BT.DOCUMENT_ITEM_NUM

                    UNION ALL
                    ----- Salesorders  portfolio_type '02','41'
                    SELECT BT.BUSINESS_ID
                          ,BT.BUSINESS_SEQUENCE
                          ,P.C_FAMILY_ID AS DOCUMENT_FAMILY_CODE
                          ,BT.CREATED_DATE
                    FROM   ODS_BUSINESS_TRANSACTIONS BT
                          ,ODS_SALE_ORDERS_ITM S    --Vista ODS_PROD_SCHEMA_SO_ITM_WOP_VW
                          ,ODS_PRODUCTS P
                    WHERE 1=1
                    AND   BT.DOC_PORTFOLIO_TYPE      IN ('02','41')
                    AND   BT.DOCUMENT_FAMILY_CODE    IS NULL
                    AND   S.PRODUCT_CODE             = P.C_PRODUCT_ID
                    AND   S.COMMERCIAL_LOCATION_CODE = BT.DOC_COMM_LOCATION_CODE
                    AND   S.SO_ORDER_YEAR            = BT.DOCUMENT_YEAR
                    AND   S.SO_ORDER_MONTH           = BT.DOCUMENT_MONTH
                    AND   TRIM (S.DOC_TYPE_CODE)     = TRIM (BT.DOC_TYPE_CODE)
                    AND   S.SO_NUMBER                = BT.DOCUMENT_NUMBER
                    AND   S.SO_ITEM_NUM              = BT.DOCUMENT_ITEM_NUM

                    UNION ALL
                    ----- Purchase portfolio_type '01', '40'
                    SELECT BT.BUSINESS_ID
                          ,BT.BUSINESS_SEQUENCE
                          ,P.C_FAMILY_ID AS DOCUMENT_FAMILY_CODE
                          ,BT.CREATED_DATE
                    FROM   ODS_BUSINESS_TRANSACTIONS BT
                          ,ODS_PURCHASE_ORDERS_ITM PUR
                          ,ODS_PRODUCTS P
                    WHERE 1=1
                    AND   BT.DOC_PORTFOLIO_TYPE        IN ('01','40')
                    AND   BT.DOCUMENT_FAMILY_CODE      IS NULL
                    AND   PUR.PRODUCT_CODE             = P.C_PRODUCT_ID
                    AND   PUR.COMMERCIAL_LOCATION_CODE = BT.DOC_COMM_LOCATION_CODE
                    AND   PUR.PO_ORDER_YEAR            = BT.DOCUMENT_YEAR
                    AND   PUR.PO_ORDER_MONTH           = BT.DOCUMENT_MONTH
                    AND   TRIM (PUR.DOC_TYPE_CODE)     = TRIM (BT.DOC_TYPE_CODE)
                    AND   PUR.PO_NUMBER                = BT.DOCUMENT_NUMBER
                    AND   PUR.PO_ITEM_NUM              = BT.DOCUMENT_ITEM_NUM

                    UNION ALL
                    ----- Replenishment portfolio_type '90'
                    SELECT BT.BUSINESS_ID
                          ,BT.BUSINESS_SEQUENCE
                          ,P.C_FAMILY_ID AS DOCUMENT_FAMILY_CODE
                          ,BT.CREATED_DATE
                     FROM  ODS_BUSINESS_TRANSACTIONS BT
                          ,ODS_REPLENISHMENT_ORDERS_ITM RO
                          ,ODS_PRODUCTS P
                     WHERE 1=1
                     AND   BT.DOC_PORTFOLIO_TYPE       IN ('90')
                     AND   BT.DOCUMENT_FAMILY_CODE     IS NULL
                     AND   RO.PRODUCT_CODE             = P.C_PRODUCT_ID
                     AND   RO.COMMERCIAL_LOCATION_CODE = BT.DOC_COMM_LOCATION_CODE
                     AND   RO.RO_ORDER_YEAR            = BT.DOCUMENT_YEAR
                     AND   RO.RO_ORDER_MONTH           = BT.DOCUMENT_MONTH
                     AND   TRIM (RO.RO_DOC_TYPE_CODE)  = TRIM (BT.DOC_TYPE_CODE)
                     AND   RO.RO_NUMBER                = BT.DOCUMENT_NUMBER
                     AND   RO.RO_ITEM_NUM              = BT.DOCUMENT_ITEM_NUM

                     UNION ALL
                     ----- Agreement  portfolio_type '50'
                     SELECT BT.BUSINESS_ID
                           ,BT.BUSINESS_SEQUENCE
                           ,P.C_FAMILY_ID AS DOCUMENT_FAMILY_CODE
                           ,BT.CREATED_DATE
                     FROM   ODS_BUSINESS_TRANSACTIONS BT
                           ,ODS_AGREEMENTS_ITM A
                           ,ODS_PRODUCTS P
                     WHERE 1=1
                     AND   BT.DOC_PORTFOLIO_TYPE            IN ('50')
                     AND   BT.DOCUMENT_FAMILY_CODE          IS NULL
                     AND   A.PRODUCT_CODE                   = P.C_PRODUCT_ID
                     AND   A.AGREEMENT_YEAR                 = BT.DOCUMENT_YEAR
                     AND   TRIM (A.AGREEMENT_DOC_TYPE_CODE) = TRIM (BT.DOC_TYPE_CODE)
                     AND   A.AGREEMENT_NUMBER               = BT.DOCUMENT_NUMBER
                     AND   A.AGREEMENT_ITEM_NUM             = BT.DOCUMENT_ITEM_NUM
             )BT1
        WHERE 1=1
        AND   (
              p_range_in_days IS NULL
              OR
              BT1.CREATED_DATE > TRUNC(SYSDATE - p_range_in_days)
              );

        v_get_family_rec         cur_get_family%ROWTYPE;
        v_count                  PLS_INTEGER;
BEGIN
         v_count:= 0;

         FOR v_get_family_rec IN cur_get_family
         LOOP
                 UPDATE  ODS_BUSINESS_TRANSACTIONS
                 SET     DOCUMENT_FAMILY_CODE    = v_get_family_rec.DOCUMENT_FAMILY_CODE
                        ,MAIN_BOUGH_BY_TYPE_FLAG = NULL
                 WHERE 1=1
                 AND   BUSINESS_ID       = v_get_family_rec.BUSINESS_ID
                 AND   BUSINESS_SEQUENCE = v_get_family_rec.BUSINESS_SEQUENCE;

                 v_count:= v_count + 1;
                 IF v_count = 2000 THEN
                    COMMIT;
                    v_count:=0;
                 END IF;
         END LOOP;
         COMMIT;
END completeness_family_product_sp;
--------------------------------------------------------------------------------

PROCEDURE update_complex_branches_bus_sp
IS
        CURSOR cur_complex_branches
        IS
        SELECT
                 BT.BUSINESS_ID
                ,BT.BUSINESS_SEQUENCE
        FROM
                ODS_BUSINESS_TRANSACTIONS BT
               ,ODS_BUSINESS_DOC_TREE_WRK W
        WHERE   1=1
        AND     BT.BUSINESS_ID = W.BUSINESS_ID
        AND     ODS_BUSN_TRX_UTILS_PKG.IS_88_OF_COMPLEX_BRANCHES_FN(
                                                                     BT.DOC_PORTFOLIO_TYPE
                                                                    ,BT.DOC_COMM_LOCATION_CODE) = ODS_COMMON_PKG.C_STR_TRUE;

        v_complex_branches_rec   cur_complex_branches%ROWTYPE;
        v_count                  PLS_INTEGER;
BEGIN
        v_count:= 0;

        FOR v_complex_branches_rec IN cur_complex_branches
        LOOP
                 UPDATE  ODS_BUSINESS_TRANSACTIONS
                 SET     SECONDARY_COSTING_FLAG = ODS_COMMON_PKG.C_STR_YES
                 WHERE   1=1
                 AND     BUSINESS_ID            = v_complex_branches_rec.BUSINESS_ID
                 AND     BUSINESS_SEQUENCE      = v_complex_branches_rec.BUSINESS_SEQUENCE;

                 v_count:= v_count + 1;
                 IF v_count = 2000 THEN
                    COMMIT;
                    v_count:=0;
                 END IF;
        END LOOP;
        COMMIT;
END update_complex_branches_bus_sp;
--------------------------------------------------------------------------------
/***************************************************************************************************/
/*********************************************************************************************
Nombre del programa:   reset_heavy_sp
    Sistema: ODS
    Objetivo: Se resetea el campo MAIN_BOUGH_BY_TYPE_FLAG de la ODS_BUSINESS_TRANSACTIONS con N
    para el BID pasado por parametro

        Parámetros de entrada:
                  business_id

    Notas:
    Autor: 15380 - Romero Alejandro
    Historia:
    Fecha               Autor                           Descripción
    20116-03-03         15380                          Creacion del SP
*********************************************************************************************/
PROCEDURE reset_heavy_sp (
            p_business_id    IN ods_business_transactions.business_id%TYPE
          )
IS
BEGIN
     UPDATE ODS_BUSINESS_TRANSACTIONS BT
     SET BT.MAIN_BOUGH_BY_TYPE_FLAG = ODS_COMMON_PKG.C_STR_NO
     WHERE 1=1
     AND   BT.BUSINESS_ID  = p_business_id;

     COMMIT;

END reset_heavy_sp;

--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   update_heavy_sp
    Sistema: ODS
    Objetivo: Se actualiza el campo MAIN_BOUGH_BY_TYPE_FLAG de la ODS_BUSINESS_TRANSACTIONS con S
    para la BID/Secuencia que se encuentra en parametro de entrada ODS_BUSN_TRX_TREE_TABLE_TYPE

        Parámetros de entrada:
                  ODS_BUSN_TRX_TREE_TABLE_TYPE

    Notas:
    Autor: 15380 - Romero Alejandro
    Historia:
    Fecha               Autor                           Descripción
    20116-03-03         15380                          Creacion del SP
*********************************************************************************************/
PROCEDURE update_heavy_sp (
          p_busn_trx_tree_table    IN OUT NOCOPY ODS_BUSN_TRX_TREE_TABLE_TYPE
          )
IS
BEGIN
		 UPDATE ODS_BUSINESS_TRANSACTIONS BT
		 SET
            BT.MAIN_BOUGH_BY_TYPE_FLAG = ODS_COMMON_PKG.C_STR_YES
     WHERE 1=1
     AND   EXISTS (
                     SELECT 1
                     FROM TABLE(CAST(p_busn_trx_tree_table AS ODS_BUSN_TRX_TREE_TABLE_TYPE)) BT2
                     WHERE 1=1
                     AND   BT.BUSINESS_ID       = BT2.ORIGINAL_BUSINESS_ID
                     AND   BT.BUSINESS_SEQUENCE = BT2.ORIGINAL_BUSINESS_SEQUENCE
                   );

     COMMIT;

END update_heavy_sp;

--------------------------------------------------------------------------------

PROCEDURE completeness_heavy_sp
IS
        CURSOR cur_busn_trx_tree_heavy_null
        IS
        SELECT
               BT.BUSINESS_ID
        FROM
               ODS_BUSINESS_TRANSACTIONS BT
        WHERE 1=1
        AND   BT.MAIN_BOUGH_BY_TYPE_FLAG IS NULL
        GROUP BY BT.BUSINESS_ID;

        v_busn_trx_tree_heavy_null_rec    cur_busn_trx_tree_heavy_null%ROWTYPE;
        v_busn_trx_tree_result_table      ODS_BUSN_TRX_TREE_TABLE_TYPE;
BEGIN
     FOR v_busn_trx_tree_heavy_null_rec IN cur_busn_trx_tree_heavy_null
     LOOP
         v_busn_trx_tree_result_table:= ODS_BUSN_TRX_UTILS_PKG.GET_BETTER_BOUGH_FN(v_busn_trx_tree_heavy_null_rec.BUSINESS_ID);

         ---Resetea la marca Heavy
         RESET_HEAVY_SP(v_busn_trx_tree_heavy_null_rec.BUSINESS_ID);

         IF v_busn_trx_tree_result_table.COUNT > 0 THEN
              ----Marca las ramas heavys por rama/tipo de cadena
               UPDATE_HEAVY_SP(v_busn_trx_tree_result_table);
         END IF;

     END LOOP;
END completeness_heavy_sp;
--------------------------------------------------------------------------------

END ODS_BUSINESS_TRANSACTION_PKG;
/
