CREATE OR REPLACE PACKAGE GBU_CALCULO_FACT_PKG IS
/*********************************************************************************************
Author  :   Pedro Dias (T53605)
Created :   2010-10-05
Purpose :   Agrupar los proceso de calculo de datos de facturacion GBU

Historial
Date            Person				Description
------------    ------------------	-------------------------------------
2010/10/05	Pedro Dias (T53605)	Creacion del Paquete
2010/10/21	Fidel Nakashima(T52585) Agregado de modulo de calculo factura de origen manual
2010/11/04	Pedro Dias (T53605)	Modificaciones en los modulos de calculo de cadena (manual y automatico)
					Agregado de modulo de replica de cadena manual y modificacion del automatico
2010/11/18	Pedro Dias (T53605)	Modificacion del modulo de replica de cadena automatica
					Eliminacion de constantes no utilizadas
                                        Modificacion de la firma de un modulo privado
2012/06/13	Pedro Dias (T53605)	Modificacion del modulo de calculo de cadena automatica
2012/06/18	Pedro Dias (T53605)	Modificacion del modulo de replica de cadena automatica
*********************************************************************************************/

-- Public type declarations
        typeCpto       VARCHAR2(4);
        typedatoorigen VARCHAR2(1);
        typedatotubo   VARCHAR2(1);

	TYPE t_rec_facturacion IS RECORD(
                f_dia_id                    	gbu_ft_facturacion.f_dia_id%TYPE,
                c_cpto_id                   	gbu_ft_facturacion.c_cpto_id%TYPE,
                c_div_id                    	gbu_ft_facturacion.c_div_id%TYPE,
                c_branch_id                 	gbu_ft_facturacion.c_branch_id%TYPE,
                c_branch_oi_id              	gbu_ft_facturacion.c_branch_oi_id%TYPE,
                c_planta_id                 	gbu_ft_facturacion.c_planta_id%TYPE,
                c_mill_oi_id                	gbu_ft_facturacion.c_mill_oi_id%TYPE,
                c_packing_dst_id            	gbu_ft_facturacion.c_packing_dst_id%TYPE,
                d_product_code              	gbu_ft_facturacion.d_product_code%TYPE,
                c_tiptubo_dst_id            	gbu_ft_facturacion.c_tiptubo_dst_id%TYPE,
                c_diametro_mm_id            	gbu_ft_facturacion.c_diametro_mm_id%TYPE,
                c_end_dst_id                	gbu_ft_facturacion.c_end_dst_id%TYPE,
                c_gr_dst_id                 	gbu_ft_facturacion.c_gr_dst_id%TYPE,
                c_norma_dst_id              	gbu_ft_facturacion.c_norma_dst_id%TYPE,
                d_espesor                   	gbu_ft_facturacion.d_espesor%TYPE,
                d_libras_pie                	gbu_ft_facturacion.d_libras_pie%TYPE,
                d_longitud_mm               	gbu_ft_facturacion.d_longitud_mm%TYPE,
                n_importe                   	gbu_ft_facturacion.n_importe%TYPE,
                n_tonnage                   	gbu_ft_facturacion.n_tonnage%TYPE,
                c_source                    	gbu_ft_facturacion.c_source%TYPE,
                c_prod_id                   	gbu_ft_facturacion.c_prod_id%TYPE,
                c_prod_ten_id               	gbu_ft_facturacion.c_prod_ten_id%TYPE,
                n_meters                    	gbu_ft_facturacion.n_meters%TYPE,
                q_quantity                  	gbu_ft_facturacion.q_quantity%TYPE,
                m_related                   	gbu_ft_facturacion.m_related%TYPE,
                c_file                      	gbu_ft_facturacion.c_file%TYPE,
                c_tubetype_std_id           	gbu_ft_facturacion.c_tubetype_std_id%TYPE,
                c_costo_mill                	gbu_ft_facturacion.c_costo_mill%TYPE,
                f_proceso                   	gbu_ft_facturacion.f_proceso%TYPE,
                c_user_id                   	gbu_ft_facturacion.c_user_id%TYPE,
                n_line                      	gbu_ft_facturacion.n_line%TYPE,
                n_productividad             	gbu_ft_facturacion.n_productividad%TYPE,
                c_rolling_id                	gbu_ft_facturacion.c_rolling_id%TYPE,
                c_protsup_dst_id            	gbu_ft_facturacion.c_protsup_dst_id%TYPE,
                c_protext_dst_id            	gbu_ft_facturacion.c_protext_dst_id%TYPE,
                c_endcli_dst_id             	gbu_ft_facturacion.c_endcli_dst_id%TYPE,
                c_puerto_dst_id             	gbu_ft_facturacion.c_puerto_dst_id%TYPE,
                c_pais_final_id             	gbu_ft_facturacion.c_pais_final_id%TYPE,
                c_cli_fac_id                	gbu_ft_facturacion.c_cli_fac_id%TYPE,
                c_branch_di_id              	gbu_ft_facturacion.c_branch_di_id%TYPE,
                f_doc_date                  	gbu_ft_facturacion.f_doc_date%TYPE,
                c_po_id                     	gbu_ft_facturacion.c_po_id%TYPE,
                c_branch_oi_ant_id          	gbu_ft_facturacion.c_branch_oi_ant_id%TYPE,
                c_branch_oi_orig_id         	gbu_ft_facturacion.c_branch_oi_orig_id%TYPE,
                c_metodo_costo_mill_id      	gbu_ft_facturacion.c_metodo_costo_mill_id%TYPE,
                c_lote_orig_id              	gbu_ft_facturacion.c_lote_orig_id%TYPE,
                c_lote_id                   	gbu_ft_facturacion.c_lote_id%TYPE,
                c_moneda                    	gbu_ft_facturacion.c_moneda%TYPE,
                n_tipo_cambio               	gbu_ft_facturacion.n_tipo_cambio%TYPE,
                c_process_id                	gbu_ft_facturacion.c_process_id%TYPE,
                c_forming_id                	gbu_ft_facturacion.c_forming_id%TYPE,
                c_flia_p_id                 	gbu_ft_facturacion.c_flia_p_id%TYPE,
                c_heat_treatment_id         	gbu_ft_facturacion.c_heat_treatment_id%TYPE,
                d_project                   	gbu_ft_facturacion.d_project%TYPE,
                c_sale_channel_branch_id    	gbu_ft_facturacion.c_sale_channel_branch_id%TYPE,
                c_sale_channel_bu_id        	gbu_ft_facturacion.c_sale_channel_bu_id%TYPE,
                c_ctr_trat_term             	gbu_ft_facturacion.c_ctr_trat_term%TYPE,
                c_prod_ctr_trat_term        	gbu_ft_facturacion.c_prod_ctr_trat_term%TYPE,
                n_importe_eur               	gbu_ft_facturacion.n_importe_eur%TYPE,
                service_equiv_inv_tn_qty    	gbu_ft_facturacion.service_equiv_inv_tn_qty%TYPE,
                supplier_subject_code       	gbu_ft_facturacion.supplier_subject_code%TYPE,
                service_facility_code       	gbu_ft_facturacion.service_facility_code%TYPE,
                service_facility_line_code  	gbu_ft_facturacion.service_facility_line_code%TYPE,
                sfso_comm_loc_code          	gbu_ft_facturacion.sfso_comm_loc_code%TYPE,
                sfso_year                   	gbu_ft_facturacion.sfso_year%TYPE,
                sfso_month                  	gbu_ft_facturacion.sfso_month%TYPE,
                sfso_doc_type_code          	gbu_ft_facturacion.sfso_doc_type_code%TYPE,
                sfso_number                 	gbu_ft_facturacion.sfso_number%TYPE,
                sfso_itm_num                	gbu_ft_facturacion.sfso_itm_num%TYPE,
                selling_po_comm_loc_code    	gbu_ft_facturacion.selling_po_comm_loc_code%TYPE,
                selling_po_year             	gbu_ft_facturacion.selling_po_year%TYPE,
                selling_po_month            	gbu_ft_facturacion.selling_po_month%TYPE,
                selling_po_doc_type_code    	gbu_ft_facturacion.selling_po_doc_type_code%TYPE,
                selling_po_number           	gbu_ft_facturacion.selling_po_number%TYPE,
                selling_po_itm_num          	gbu_ft_facturacion.selling_po_itm_num%TYPE,
                svo_comm_loc_code           	gbu_ft_facturacion.svo_comm_loc_code%TYPE,
                svo_year                    	gbu_ft_facturacion.svo_year%TYPE,
                svo_month                   	gbu_ft_facturacion.svo_month%TYPE,
                svo_doc_type_code           	gbu_ft_facturacion.svo_doc_type_code%TYPE,
                svo_number                  	gbu_ft_facturacion.svo_number%TYPE,
                svo_itm_num                 	gbu_ft_facturacion.svo_itm_num%TYPE,
                svo_type                    	gbu_ft_facturacion.svo_type%TYPE,
                sv_so_comm_loc_code_prv     	gbu_ft_facturacion.sv_so_comm_loc_code_prv%TYPE,
                sv_so_year_prv              	gbu_ft_facturacion.sv_so_year_prv%TYPE,
                sv_so_month_prv             	gbu_ft_facturacion.sv_so_month_prv%TYPE,
                sv_so_doc_type_code_prv     	gbu_ft_facturacion.sv_so_doc_type_code_prv%TYPE,
                sv_so_number_prv            	gbu_ft_facturacion.sv_so_number_prv%TYPE,
                sv_so_itm_num_prv           	gbu_ft_facturacion.sv_so_itm_num_prv%TYPE,
                n_real_tonnage			gbu_ft_facturacion.n_real_tonnage%TYPE,
                business_id_ref			gbu_ft_facturacion.business_id_ref%TYPE,
                sap_company_code		gbu_ft_facturacion.sap_company_code%TYPE
        	);

-- Public constant declarations
-- /* conceptos de precio base neto */
-- cptoproductobn                CONSTANT typecpto%TYPE         := '0101';
-- cptobn_dst_tubular            CONSTANT typecpto%TYPE         := '0090';
-- cptobn_nodst_tubular          CONSTANT typecpto%TYPE         := '0091';
-- cptobn_dst_notubular          CONSTANT typecpto%TYPE         := '0092';
-- cptobn_nodst_notubular        CONSTANT typecpto%TYPE         := '0093';
-- cptopbn_nodefdst              CONSTANT typecpto%TYPE         := '0050';
-- cptopbn_nodefnodst            CONSTANT typecpto%TYPE         := '0051';
-- /* conceptos de servicios */
-- cptoPrecioServicio            constant typeCpto%type         := '0102';
-- cptoServicio_DST_tubular      constant typeCpto%type         := '0080';
-- cptoServicio_noDST_Tubular    constant typeCpto%type         := '0081';
-- cptoServicio_DST_noTubular    constant typeCpto%type         := '0082';
-- cptoServicio_noDST_noTubular  constant typeCpto%type         := '0083';
-- cptoSERVICIO_NoDefDST         constant typeCpto%type         := '0054';
-- cptoSERVICIO_NoDefNoDST       constant typeCpto%type         := '0055';
-- /* conceptos de fletes */
-- cptoproductoflete             CONSTANT typecpto%TYPE         := '0103';
-- cptoflete_dst_tubular         CONSTANT typecpto%TYPE         := '0094';
-- cptoflete_nodst_tubular       CONSTANT typecpto%TYPE         := '0095';
-- cptoflete_dst_notubular       CONSTANT typecpto%TYPE         := '0096';
-- cptoflete_nodst_notubular     CONSTANT typecpto%TYPE         := '0097';
-- cptoflete_nodefdst            CONSTANT typecpto%TYPE         := '0052';
-- cptoflete_nodefnodst          CONSTANT typecpto%TYPE         := '0053';
-- /* conceptos de Bonificadion facturacion */
-- cptobonif                     CONSTANT typecpto%TYPE         := '0104';
-- cptobonif_dst_tubular         CONSTANT typecpto%TYPE         := '0056';
-- cptobonif_nodst_tubular       CONSTANT typecpto%TYPE         := '0057';
-- cptobonif_dst_notubular       CONSTANT typecpto%TYPE         := '0058';
-- cptobonif_nodst_notubular     CONSTANT typecpto%TYPE         := '0059';
-- cptobonif_nodefdst            CONSTANT typecpto%TYPE         := '0048';
-- cptobonif_nodefnodst          CONSTANT typecpto%TYPE         := '0049';
-- /*  Conceptos Especiales de Precios -  anomalias  */
-- cptoproductoesp               CONSTANT typecpto%TYPE         := 'ESPE';
-- cptoprecio_nodefdst           CONSTANT typecpto%TYPE         := '0085';
-- cptoprecio_nodefnodst         CONSTANT typecpto%TYPE         := '0086';
-- /* concepto de precio JIT */
-- cptopreciojit            CONSTANT typecpto%TYPE         := '0106';
-- cptojit_dst_tubular      CONSTANT typecpto%TYPE         := '0042';
-- cptojit_nodst_tubular    CONSTANT typecpto%TYPE         := '0043';
-- cptojit_dst_notubular    CONSTANT typecpto%TYPE         := '0044';
-- cptojit_nodst_notubular  CONSTANT typecpto%TYPE         := '0045';
-- cptojit_nodefdst         CONSTANT typecpto%TYPE         := '0046';
-- cptojit_nodefnodst       CONSTANT typecpto%TYPE         := '0047';
-- /* Invoicing: Interests */
-- cptointeres                 CONSTANT typecpto%TYPE         := '0107';
-- cptointerestubdst           CONSTANT typecpto%TYPE         := '0034';
-- cptointerestubnodst         CONSTANT typecpto%TYPE         := '0035';
-- cptointerestsnotubdst       CONSTANT typecpto%TYPE         := '0036';
-- cptointerestsnotubnodst     CONSTANT typecpto%TYPE         := '0037';
-- cptointerestsindefdst       CONSTANT typecpto%TYPE         := '0038';
-- cptointerestsindefnodst     CONSTANT typecpto%TYPE         := '0039';


    /* Concepto de Costo Standart de Producto */
    cptoCostoSTD                  constant typeCpto%type         := '0201';
    cptoCostoSTD_tercero          constant typeCpto%type         := '0202';
    cptoCostoSTD_DST_Tubular      constant typeCpto%type         := '0060';
    cptoCostoSTD_noDST_Tubular    constant typeCpto%type         := '0061';
    cptoCostoSTD_DST_noTubular    constant typeCpto%type         := '0062';
    cptoCostoSTD_noDST_noTubular  constant typeCpto%type         := '0063';
    /*  Conceptos Especiales de Costos -  anomalias  */
    cptoCosto_NoDefDST            constant typeCpto%type         := '0064';
    cptoCosto_NoDefNoDST          constant typeCpto%type         := '0065';
--
-- /* origenes de los datos */
-- origendst                     CONSTANT typedatoorigen%TYPE   := '1';
-- origennodst                   CONSTANT typedatoorigen%TYPE   := '2';
-- origenerror                   CONSTANT typedatoorigen%TYPE   := '3';
-- /* tipos de tubos de los datos */
-- prodtubular                   CONSTANT typedatotubo%TYPE     := 'W';
-- prodnotubular                 CONSTANT typedatotubo%TYPE     := 'X';
-- prodtuberror_notdef           CONSTANT typedatotubo%TYPE     := 'Y';
-- prodtubular_de_servicio       CONSTANT typedatotubo%TYPE     := 'Z';

 c_undefined_oi_id	CONSTANT gbu_ft_facturacion.c_branch_oi_id%TYPE := 'UND-00000000000000-00000';

-- Public variable declarations

-- Public function and procedure declarations

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   calculo_cadena_origen_sap_sp
	Sistema: GBU
	Objetivo: calculo de datos de facturacion GBU para datos de facturacion de origen SAP


	Parámetros de entrada:
		N/A

	Parámetros de salida:
		N/A

	Notas:
	Autor: <Autor ID>
	Historia:
	Fecha		Autor		Descripción
	----------------------------------------------------------------------
	2010/10/21	t52585		Creacion del modulo
        2010/11/04	T53605		Modificacion del modulo para que solo sean actualizadas las cadenas
        				que fueron modificadas respecto de la carga existente. Se utiliza la misma
                                        funcion del proceso actual de calculo de cadena (gbu_ods_calculos_pkg)
	2012/06/13	T53605		Modificacion del cursor del sp, utiliza un cursor dinamico para cumplir con el requerimiento de que
        				solo sean modificados los origenes que ingrese el usuario en la GBU_SOLICITUD, en caso de que no haya
                                        cargado ninguno el SP no tiene en cuenta el filtro a la GBU_SOLICITUD

*********************************************************************************************/
   --
   PROCEDURE calculo_cadena_origen_sap_sp;


----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   calculo_cadena_origen_man_sp
	Sistema: GBU
	Objetivo: calcular datos no generados por el proceso de carga para los datos
        de origen de carga manual

	Parámetros de entrada:
		p_source: se especifica el origen manual

	Parámetros de salida:
		N/A

	Notas:
	Autor: T52585
	Historia:
	Fecha		Autor		Descripción
        ----------------------------------------------------------------------
	2010/10/21	t52585		Creacion del modulo
        2010/11/04	T53605		Modificacion, solo seran actualizadas las cadenas
        				que fueron modificadas respecto de la carga existente. Se utiliza la misma
                                        funcion del proceso actual de calculo de cadena (gbu_ods_calculos_pkg).
                                        Se incluye al proceso una nueva tabla que contiene las branch de carga manual

*********************************************************************************************/
   --
   PROCEDURE calculo_cadena_origen_man_sp;

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   calculo_cadena_origen_a_pedido_sp
	Sistema: GBU
	Objetivo: calculo de datos de facturacion GBU para datos de facturacion que se realizo a pedido por SOC o PEP


	Parámetros de entrada:
		N/A

	Parámetros de salida:
		N/A

	Notas:
	Autor: <Autor ID>
	Historia:
	Fecha		Autor		Descripción
	----------------------------------------------------------------------
	2015/03/08	S15380		Creacion del modulo

*********************************************************************************************/
   --
   PROCEDURE calculo_cadena_a_pedido_sp;

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   replica_costos_origen_sap_sp
	Sistema: GBU
	Objetivo: genera la replica de los conceptos Costo Social y Costo Social de Terceros
        en a tabla de facturacion GBU para datos de facturacion de origen SAP

	Parámetros de entrada:
		N/A

	Parámetros de salida:
		N/A

	Notas:
	Autor: T52585
	Historia:
	Fecha		Autor		Descripción
	2010/10/25	T52585		Creacion de Modulo
	2012/06/18	T53605		Modificacion del cursor del sp, utiliza un cursor dinamico para cumplir con el requerimiento de que
        				solo sean modificados los origenes que ingrese el usuario en la GBU_SOLICITUD, en caso de que no haya
                                        cargado ninguno el SP no tiene en cuenta el filtro a la GBU_SOLICITUD


*********************************************************************************************/
PROCEDURE replica_costos_origen_sap_sp;

----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   replica_costos_origen_man_sp
	Sistema: GBU
	Objetivo: genera la replica de los conceptos Costo Social y Costo Social de Terceros
        en a tabla de facturacion GBU para datos de facturacion de origen Manual

	Parámetros de entrada:
		N/A

	Parámetros de salida:
		N/A

	Notas:
	Autor: T53605
	Historia:
	Fecha		Autor		Descripción
	2010/10/25	T53605		Creacion de Modulo
        2010/11/04	T53605		Modificacion, solo seran actualizadas las cadenas
        				que fueron modificadas respecto de la carga existente. Se utiliza la misma
                                        funcion del proceso actual de calculo de cadena (gbu_ods_calculos_pkg).
                                        Se incluye al proceso una nueva tabla que contiene las branch de carga manual
					donde seran logeadas las corridas.
        2010/11/04	T53605		Modificacion en la actualizacion de gbu_manual_process_wrk

*********************************************************************************************/
PROCEDURE replica_costos_origen_man_sp;
----------------------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa:   replica_costos_a_pedido_sp
	Sistema: GBU
	Objetivo: genera la replica de los conceptos Costo Social y Costo Social de Terceros
        en a tabla de facturacion GBU para datos de facturacion que se realizo a pedido por SOC o PEP

	Parámetros de entrada:
		N/A

	Parámetros de salida:
		N/A

	Notas:
	Autor: S15380
	Historia:
	Fecha		Autor		Descripción
*********************************************************************************************/
PROCEDURE replica_costos_a_pedido_sp;

END gbu_calculo_fact_pkg;
/
CREATE OR REPLACE PACKAGE BODY GBU_CALCULO_FACT_PKG IS
-- Private type declarations
  --Registro con la PO a Calcular
  TYPE typ_reg_po IS RECORD (supplier_subject_code      gbu_facturas.supplier_subject_code%TYPE,
                             selling_po_comm_loc_code   gbu_facturas.selling_po_comm_loc_code%TYPE,
                             selling_po_year            gbu_facturas.selling_po_year%TYPE,
                             selling_po_month           gbu_facturas.selling_po_month%TYPE,
                             selling_po_doc_type_code   gbu_facturas.selling_po_doc_type_code%TYPE,
                             selling_po_number          gbu_facturas.selling_po_number%TYPE,
                             selling_po_itm_num         gbu_facturas.selling_po_itm_num%TYPE);

  TYPE t_rec_cadena_sap IS RECORD (
  				c_branch_id                 	gbu_ft_facturacion.c_branch_id%TYPE,
                		c_branch_oi_id              	gbu_ft_facturacion.c_branch_oi_id%TYPE,
                                c_branch_oi_ant_id          	gbu_ft_facturacion.c_branch_oi_ant_id%TYPE,
                                c_mill_oi_id                	gbu_ft_facturacion.c_mill_oi_id%TYPE,
                                c_po_id                     	gbu_ft_facturacion.c_po_id%TYPE,
                		selling_po_comm_loc_code    	gbu_ft_facturacion.selling_po_comm_loc_code%TYPE,
                                selling_po_doc_type_code    	gbu_ft_facturacion.selling_po_doc_type_code%TYPE,
                                selling_po_itm_num          	gbu_ft_facturacion.selling_po_itm_num%TYPE,
                                selling_po_month            	gbu_ft_facturacion.selling_po_month%TYPE,
                                selling_po_number           	gbu_ft_facturacion.selling_po_number%TYPE,
                                selling_po_year             	gbu_ft_facturacion.selling_po_year%TYPE,
                                supplier_subject_code       	gbu_ft_facturacion.supplier_subject_code%TYPE,
                                f_dia_id                    	gbu_ft_facturacion.f_dia_id%TYPE,
                                row_id				rowid
  				   );

  SUBTYPE t_process_name IS VARCHAR2(8);

  TYPE t_ref_cur IS REF CURSOR;

-- Private constant declarations

	c_proceso_carga t_process_name := 'CARGA';
        c_proceso_cadena t_process_name := 'CADENA';
        c_proceso_replica t_process_name := 'REPLICA';
        c_proceso_ic_replica t_process_name := 'IC-R';
        c_proceso_ajuste t_process_name := 'AJU_COS';

        c_proceso_pedido t_process_name        := 'PEDIDO';
        c_proceso_cadena_pedido t_process_name := 'PEDIDO-C';

	c_sap_source CONSTANT VARCHAR2(3):= 'SAP';
        c_manual_source CONSTANT VARCHAR2(3):= 'MAN';

        c_metodo_costo_mill_replica CONSTANT NUMBER(3):= 1;

        c_last_updated_by CONSTANT VARCHAR2(10):= 'DATASTAGE';

-- Private variable declarations

-- Function and procedure implementations

--////////////////// PRIVATE MODULES SPECIFICATION ////////////////////////

 /*@ARO 12/12/2014*/
FUNCTION GET_MILL_AJUST_CONFERMAS_FN(
    P_F_DIA_ID            IN GBU_AJUST_CONFERMAS_WRK.F_DIA_ID%TYPE,
    P_C_ORDER_ITEMS_ID    IN GBU_AJUST_CONFERMAS_WRK.C_ORDER_ITEMS_ID%TYPE,
    P_N_LINE              IN GBU_AJUST_CONFERMAS_WRK.N_LINE%TYPE,
    P_C_FILE              IN GBU_AJUST_CONFERMAS_WRK.C_FILE%TYPE
    )
RETURN GBU_AJUST_CONFERMAS_WRK.C_MILL_OI_ID%TYPE;
 /*@ARO 12/12/2014*/

FUNCTION gbu_get_ssc_fn(
	p_reg_po    IN typ_reg_po
        )
RETURN 	gbu_facturas.supplier_subject_code%TYPE;

--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa: borrar_conceptos_replica_sp
	Sistema: GBU
	Objetivo: borra los conceptos generados por el proceso de replicacion
        correspondientes a un periodo especificado

	Parámetros de entrada:
		p_fecha_id : fecha a la que pertenece el periodo para el cual se
                borrarán los conceptos generados por el proceso de replicacion
                p_source: Origen de la replica, Manual o Automatico (SAP)
                p_exist: Indica si existe en la tabla GBU_SOLICITUD. 1 existe dato en la tabla, 0 no existe

	Parámetros de salida:
		N/A

	Notas:
	Autor: T52585
	Historia:
	Fecha		Autor		Descripción
	2010/10/25	T52585		Creacion de Modulo
        2010/11/04	T53605		Se agrego el parametro p_source para moduralizar la logica del codigo
        2012/06/18	T53605		Se agrego un parametro, p_exist, que verifica si existen branch en gbu_solicitud para que solo
        				se eliminen las de las tabla si es que existe, sino borra todas.

*********************************************************************************************/
PROCEDURE borrar_conceptos_replica_sp(
	p_dia_id	gbu_ft_facturacion.f_dia_id%TYPE,
        p_source	gbu_ft_facturacion.c_source%TYPE,
        p_exist		IN PLS_INTEGER DEFAULT 0
        );


--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa: replica_concepto_costo_std_sp
	Sistema: GBU
	Objetivo: dado un registro de facturacion GBU de concepto costo social o costo social de terceros
        se genera un registro de facturacion FBU con el concepto costo estandard de produccion DST

	Parámetros de entrada:
		p_concepto_fact_orig: registro correspondiente al concepto de costo social a replicar
                como concepto de costo estandad
                p_process_date:fecha de procesamiento

	Parámetros de salida:
		N/A

	Notas:
	Autor: T52585
	Historia:
	Fecha		Autor		Descripción
	2010/10/25	T52585		Creacion de Modulo

*********************************************************************************************/
PROCEDURE replica_concepto_costo_std_sp(
	p_concepto_fact_orig	t_rec_facturacion,
        p_process_date		DATE
        );


--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa: existe_concepto_sesenta_fn
	Sistema: GBU
	Objetivo: dado una fecha de facturacion y el branch_oi_id se verifica si dentro de los conceptos
        existe al menos 1 proveniente de sap (familia de conceptos 0060). Si es asi retorna TRUE, y FALSE
        en caso contrario

	Parámetros de entrada:
		p_branch_oi_id: identificador branch oi
                p_dia_id:fecha de facturacion

	Parámetros de salida:
		salida funcion: TRUE en caso que exista un concepto provieniente de sap y FALSE en caso contrario

	Notas:
	Autor: T52585
	Historia:
	Fecha		Autor		Descripción
	2010/10/25	T52585		Creacion de Modulo
        2010/11/18	T53605		Por requerimiento funcional se modifico el nombre de la funcion

*********************************************************************************************/
FUNCTION existe_concepto_sesenta_fn(
	p_branch_oi_id		gbu_ft_facturacion.c_branch_oi_id%TYPE,
        p_dia_id		gbu_ft_facturacion.f_dia_id%TYPE
        )
RETURN BOOLEAN;


--------------------------------------------------------------------------------
/*********************************************************************************************
Nombre del programa: branch_related_mill_fn
	Sistema: GBU
	Objetivo: dado un branch y una milla retorna TRUE si estan relacionadas y
        FALSE en caso contrario  (verifica si existe una relacion en la tabla GBU_LK_SOC_CON_RFS)

	Parámetros de entrada:
		p_branch_id: identificador branch oi
                p_mill_id:identificador mill id

	Parámetros de salida:
		salida funcion: TRUE en caso que exista un a relacion y FALSE en caso contrario

	Notas:
	Autor: T52585
	Historia:
	Fecha		Autor		Descripción
	2010/10/25	T52585		Creacion de Modulo

*********************************************************************************************/
FUNCTION branch_related_mill_fn (
	p_branch_id 	gbu_ft_facturacion.c_branch_id%TYPE,
	p_mill_id 	gbu_ft_facturacion.c_planta_id%TYPE
        )
RETURN BOOLEAN;

--------------------------------------------------------------------------------
--////////////////// PRIVATE MODULES IMPLEMENTATION ////////////////////////

/*@ARO 12/12/2014*/
FUNCTION GET_MILL_AJUST_CONFERMAS_FN(
    P_F_DIA_ID            IN GBU_AJUST_CONFERMAS_WRK.F_DIA_ID%TYPE,
    P_C_ORDER_ITEMS_ID    IN GBU_AJUST_CONFERMAS_WRK.C_ORDER_ITEMS_ID%TYPE,
    P_N_LINE              IN GBU_AJUST_CONFERMAS_WRK.N_LINE%TYPE,
    P_C_FILE              IN GBU_AJUST_CONFERMAS_WRK.C_FILE%TYPE
    )
RETURN GBU_AJUST_CONFERMAS_WRK.C_MILL_OI_ID%TYPE
IS
    V_C_MILL_OI_ID GBU_AJUST_CONFERMAS_WRK.C_MILL_OI_ID%TYPE;
BEGIN
    BEGIN
        SELECT
                C_MILL_OI_ID
        INTO
                V_C_MILL_OI_ID
        FROM
                GBU_AJUST_CONFERMAS_WRK
        WHERE   F_DIA_ID         = P_F_DIA_ID
        AND     C_ORDER_ITEMS_ID = P_C_ORDER_ITEMS_ID
        AND     N_LINE           = P_N_LINE
        AND     C_FILE           = P_C_FILE
        AND     DELETED_DATE     IS NULL;


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            V_C_MILL_OI_ID := '000-00000000000000-00000';

        WHEN TOO_MANY_ROWS THEN
            V_C_MILL_OI_ID := '000-00000000000000-00000';
    END;

    RETURN V_C_MILL_OI_ID;

END GET_MILL_AJUST_CONFERMAS_FN;
 /*@ARO 12/12/2014*/

FUNCTION gbu_get_ssc_fn(
    p_reg_po    IN typ_reg_po)
RETURN gbu_facturas.supplier_subject_code%TYPE
IS
    v_supplier_subject_code gbu_facturas.supplier_subject_code%TYPE;
BEGIN
    BEGIN
        SELECT
                h.supplier_subject_code
        INTO
                v_supplier_subject_code
        FROM
                ods_purchase_orders_hdr h
        WHERE   h.commercial_location_code = p_reg_po.selling_po_comm_loc_code
        AND     h.po_order_year            = p_reg_po.selling_po_year
        AND     h.po_order_month           = p_reg_po.selling_po_month
        AND     h.doc_type_code            = TRIM(p_reg_po.selling_po_doc_type_code)
        AND     h.po_number                = p_reg_po.selling_po_number;


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            v_supplier_subject_code := NULL;
    END;

    RETURN v_supplier_subject_code;

END gbu_get_ssc_fn;
--------------------------------------------------------------------------------

PROCEDURE borrar_conceptos_replica_sp(
	p_dia_id	IN gbu_ft_facturacion.f_dia_id%TYPE,
        p_source	IN gbu_ft_facturacion.c_source%TYPE,
        p_exist		IN PLS_INTEGER DEFAULT 0
        )
IS
BEGIN
	DELETE 	gbu_ft_facturacion ff
        WHERE	0=0
        AND	ff.f_dia_id = p_dia_id
        AND	ff.c_source = p_source
        AND	ff.c_user_id IN (c_proceso_replica, c_proceso_ic_replica)
	AND	(	(p_source = c_manual_source AND EXISTS (SELECT  1
								FROM    gbu_manual_process_wrk mp
								WHERE   0=0
                                                                AND     mp.f_dia_id = ff.f_dia_id
                                                                AND     mp.c_branch_id = ff.c_branch_id
                                                                AND     mp.c_estado = c_proceso_replica
                                                                )
			)
		OR 	(p_source = c_sap_source AND ((p_exist = 1 AND EXISTS (SELECT 1
                                                   			 FROM   gbu_solicitud s
                                                                   	 WHERE  0=0
                                                                   	 AND    s.f_dia_id = ff.f_dia_id
                                                                         AND	s.c_branch_id = ff.c_branch_id
                                                                   	 AND    s.marca_proceso = 'D'
                                                                   	 AND    s.c_proceso = 'CFS'
                                                                   	 ))
	   				               OR p_exist = 0)
                        )
                );

END borrar_conceptos_replica_sp;

--------------------------------------------------------------------------------

PROCEDURE replica_concepto_costo_std_sp(
	p_concepto_fact_orig	t_rec_facturacion,
        p_process_date		DATE
        )
IS
BEGIN
	INSERT INTO gbu_ft_facturacion(
                f_dia_id		,
                c_cpto_id               ,
                c_div_id                ,
                c_branch_id             ,
                c_branch_oi_id          ,
                c_planta_id             ,
                c_mill_oi_id            ,
                c_packing_dst_id        ,
                d_product_code          ,
                c_tiptubo_dst_id        ,
                c_diametro_mm_id        ,
                c_end_dst_id            ,
                c_gr_dst_id             ,
                c_norma_dst_id          ,
                d_espesor               ,
                d_libras_pie            ,
                d_longitud_mm           ,
                n_importe               ,
                n_tonnage               ,
                c_source                ,
                c_prod_id               ,
                c_prod_ten_id           ,
                n_meters                ,
                q_quantity              ,
                m_related               ,
                c_file                  ,
                c_tubetype_std_id       ,
                c_costo_mill            ,
                f_proceso               ,
                c_user_id               ,
                n_line                  ,
                n_productividad         ,
                c_rolling_id            ,
                c_protsup_dst_id        ,
                c_protext_dst_id        ,
                c_endcli_dst_id         ,
                c_puerto_dst_id         ,
                c_pais_final_id         ,
                c_cli_fac_id            ,
                c_branch_di_id          ,
                f_doc_date              ,
                c_po_id                 ,
                c_branch_oi_ant_id      ,
                c_branch_oi_orig_id     ,
                c_metodo_costo_mill_id  ,
                c_lote_orig_id          ,
                c_lote_id               ,
                c_moneda                ,
                n_tipo_cambio           ,
                c_process_id            ,
                c_forming_id            ,
                c_flia_p_id             ,
                c_heat_treatment_id     ,
                d_project               ,
                c_sale_channel_branch_id,
                c_sale_channel_bu_id    ,
                c_ctr_trat_term         ,
                c_prod_ctr_trat_term    ,
                n_importe_eur           ,
                service_equiv_inv_tn_qty,
                supplier_subject_code   ,
                service_facility_code   ,
                service_facility_line_code,
                sfso_comm_loc_code      ,
                sfso_year               ,
                sfso_month              ,
                sfso_doc_type_code      ,
                sfso_number             ,
                sfso_itm_num            ,
                selling_po_comm_loc_code,
                selling_po_year         ,
                selling_po_month        ,
                selling_po_doc_type_code,
                selling_po_number       ,
                selling_po_itm_num      ,
                svo_comm_loc_code       ,
                svo_year                ,
                svo_month               ,
                svo_doc_type_code       ,
                svo_number              ,
                svo_itm_num             ,
                svo_type                ,
                sv_so_comm_loc_code_prv ,
                sv_so_year_prv          ,
                sv_so_month_prv         ,
                sv_so_doc_type_code_prv ,
                sv_so_number_prv        ,
                sv_so_itm_num_prv	,
                n_real_tonnage		,
                business_id_ref		,
                sap_company_code
        	)
        VALUES (
                p_concepto_fact_orig.f_dia_id		,
                cptoCostoSTD_DST_Tubular               ,    -- concepto costo estandard
                p_concepto_fact_orig.c_div_id                ,
                p_concepto_fact_orig.c_branch_id             ,
                p_concepto_fact_orig.c_branch_oi_id          ,
                p_concepto_fact_orig.c_planta_id             ,
                p_concepto_fact_orig.c_mill_oi_id            ,
                p_concepto_fact_orig.c_packing_dst_id        ,
                p_concepto_fact_orig.d_product_code          ,
                p_concepto_fact_orig.c_tiptubo_dst_id        ,
                p_concepto_fact_orig.c_diametro_mm_id        ,
                p_concepto_fact_orig.c_end_dst_id            ,
                p_concepto_fact_orig.c_gr_dst_id             ,
                p_concepto_fact_orig.c_norma_dst_id          ,
                p_concepto_fact_orig.d_espesor               ,
                p_concepto_fact_orig.d_libras_pie            ,
                p_concepto_fact_orig.d_longitud_mm           ,
                p_concepto_fact_orig.n_importe               ,
                p_concepto_fact_orig.n_tonnage               ,
                p_concepto_fact_orig.c_source                ,
                p_concepto_fact_orig.c_prod_id               ,
                p_concepto_fact_orig.c_prod_ten_id           ,
                p_concepto_fact_orig.n_meters                ,
                p_concepto_fact_orig.q_quantity              ,
                p_concepto_fact_orig.m_related               ,
                p_concepto_fact_orig.c_file                  ,
                p_concepto_fact_orig.c_tubetype_std_id       ,
                p_concepto_fact_orig.c_costo_mill            ,
                p_process_date               		     , -- Fecha de procesamiento
                c_proceso_replica                            , -- nombre del proceso REPLICA
                p_concepto_fact_orig.n_line                  ,
                p_concepto_fact_orig.n_productividad         ,
                p_concepto_fact_orig.c_rolling_id            ,
                p_concepto_fact_orig.c_protsup_dst_id        ,
                p_concepto_fact_orig.c_protext_dst_id        ,
                p_concepto_fact_orig.c_endcli_dst_id         ,
                p_concepto_fact_orig.c_puerto_dst_id         ,
                p_concepto_fact_orig.c_pais_final_id         ,
                p_concepto_fact_orig.c_cli_fac_id            ,
                p_concepto_fact_orig.c_branch_di_id          ,
                p_concepto_fact_orig.f_doc_date              ,
                p_concepto_fact_orig.c_po_id                 ,
                p_concepto_fact_orig.c_branch_oi_ant_id      ,
                p_concepto_fact_orig.c_branch_oi_orig_id     ,
                c_metodo_costo_mill_replica                  ,   -- metodo de costo replica
                p_concepto_fact_orig.c_lote_orig_id          ,
                p_concepto_fact_orig.c_lote_id               ,
                p_concepto_fact_orig.c_moneda                ,
                p_concepto_fact_orig.n_tipo_cambio           ,
                p_concepto_fact_orig.c_process_id            ,
                p_concepto_fact_orig.c_forming_id            ,
                p_concepto_fact_orig.c_flia_p_id             ,
                p_concepto_fact_orig.c_heat_treatment_id     ,
                p_concepto_fact_orig.d_project               ,
                p_concepto_fact_orig.c_sale_channel_branch_id,
                p_concepto_fact_orig.c_sale_channel_bu_id    ,
                p_concepto_fact_orig.c_ctr_trat_term         ,
                p_concepto_fact_orig.c_prod_ctr_trat_term    ,
                p_concepto_fact_orig.n_importe_eur           ,
                p_concepto_fact_orig.service_equiv_inv_tn_qty,
                p_concepto_fact_orig.supplier_subject_code   ,
                p_concepto_fact_orig.service_facility_code   ,
                p_concepto_fact_orig.service_facility_line_code,
                p_concepto_fact_orig.sfso_comm_loc_code      ,
                p_concepto_fact_orig.sfso_year               ,
                p_concepto_fact_orig.sfso_month              ,
                p_concepto_fact_orig.sfso_doc_type_code      ,
                p_concepto_fact_orig.sfso_number             ,
                p_concepto_fact_orig.sfso_itm_num            ,
                p_concepto_fact_orig.selling_po_comm_loc_code,
                p_concepto_fact_orig.selling_po_year         ,
                p_concepto_fact_orig.selling_po_month        ,
                p_concepto_fact_orig.selling_po_doc_type_code,
                p_concepto_fact_orig.selling_po_number       ,
                p_concepto_fact_orig.selling_po_itm_num      ,
                p_concepto_fact_orig.svo_comm_loc_code       ,
                p_concepto_fact_orig.svo_year                ,
                p_concepto_fact_orig.svo_month               ,
                p_concepto_fact_orig.svo_doc_type_code       ,
                p_concepto_fact_orig.svo_number              ,
                p_concepto_fact_orig.svo_itm_num             ,
                p_concepto_fact_orig.svo_type                ,
                p_concepto_fact_orig.sv_so_comm_loc_code_prv ,
                p_concepto_fact_orig.sv_so_year_prv          ,
                p_concepto_fact_orig.sv_so_month_prv         ,
                p_concepto_fact_orig.sv_so_doc_type_code_prv ,
                p_concepto_fact_orig.sv_so_number_prv        ,
                p_concepto_fact_orig.sv_so_itm_num_prv	     ,
                p_concepto_fact_orig.n_real_tonnage	     ,
                p_concepto_fact_orig.business_id_ref	     ,
                p_concepto_fact_orig.sap_company_code
                );

END replica_concepto_costo_std_sp;

--------------------------------------------------------------------------------

FUNCTION existe_concepto_sesenta_fn(
	p_branch_oi_id		gbu_ft_facturacion.c_branch_oi_id%TYPE,
        p_dia_id		gbu_ft_facturacion.f_dia_id%TYPE
        )
RETURN BOOLEAN
IS
	v_result	BOOLEAN:= TRUE;
        v_dummy		PLS_INTEGER;
BEGIN
	BEGIN
        	SELECT	1
                INTO	v_dummy
                FROM	gbu_ft_facturacion
                WHERE	c_branch_oi_id = p_branch_oi_id
                AND	f_dia_id = p_dia_id
                AND	c_cpto_id IN (
                        cptoCostoSTD_DST_Tubular,
                        cptoCostoSTD_noDST_Tubular,
                        cptoCostoSTD_DST_noTubular,
                        cptoCostoSTD_noDST_noTubular,
                        cptoCosto_NoDefDST,
                        cptoCosto_NoDefNoDST
                        )
		AND	ROWNUM = 1;
	EXCEPTION
        	WHEN no_data_found
                THEN
                	v_result:= FALSE;
        END;

	RETURN v_result;
END existe_concepto_sesenta_fn;

--------------------------------------------------------------------------------

FUNCTION branch_related_mill_fn (
	p_branch_id 	gbu_ft_facturacion.c_branch_id%TYPE,
	p_mill_id 	gbu_ft_facturacion.c_planta_id%TYPE
        )
RETURN BOOLEAN
IS
	v_dummy	PLS_INTEGER;
        v_result BOOLEAN:= TRUE;

BEGIN
	BEGIN
        	SELECT	1
                INTO	v_dummy
                FROM	gbu_lk_soc_con_rfs
                WHERE	c_branch_id = p_branch_id
                AND	c_mill_id = p_mill_id;
	EXCEPTION
        	WHEN no_data_found
                THEN
                	v_result:= FALSE;
        END;

	RETURN v_result;
END branch_related_mill_fn;

--------------------------------------------------------------------------------
--////////////////// PUBLIC MODULES IMPLEMENTATION ////////////////////////

PROCEDURE calculo_cadena_origen_sap_sp IS
/* ---------------------------------------------------------- */
/* Declaraciones de constantes, variables y cursor generales  */
/* ---------------------------------------------------------- */



--        CURSOR cur_fact IS
--        SELECT 	ff.c_branch_id,
--                ff.c_branch_oi_id,
--                ff.c_branch_oi_ant_id,
--                ff.c_mill_oi_id,
--                ff.c_po_id,
--                ff.selling_po_comm_loc_code,
--                ff.selling_po_doc_type_code,
--                ff.selling_po_itm_num,
--                ff.selling_po_month,
--                ff.selling_po_number,
--                ff.selling_po_year,
--                ff.supplier_subject_code,
--                ff.f_dia_id,
--                ff.rowid
--        FROM   	gbu_ft_facturacion ff,
--                gbu_estado_periodo_vig ep
--        WHERE  	0=0
--        AND    	ff.f_dia_id = ep.f_dia_id
--        AND    	ep.c_file = 'F'
--        AND    	ep.c_estado = 'C'
--        AND	ff.c_source = c_sap_source
--        ORDER BY ff.c_branch_oi_id;
        --
        c_cursor      CONSTANT 	VARCHAR2(2000) := 'SELECT ff.c_branch_id,
                                                          ff.c_branch_oi_id,
                                                          ff.c_branch_oi_ant_id,
                                                          ff.c_mill_oi_id,
                                                          ff.c_po_id,
                                                          ff.selling_po_comm_loc_code,
                                                          ff.selling_po_doc_type_code,
                                                          ff.selling_po_itm_num,
                                                          ff.selling_po_month,
                                                          ff.selling_po_number,
                                                          ff.selling_po_year,
                                                          ff.supplier_subject_code,
                                                          ff.f_dia_id,
                                                          ff.rowid
                                                   FROM   gbu_ft_facturacion ff,
                                                          gbu_estado_periodo_vig ep
                                                   WHERE  0=0
                                                   AND    ff.f_dia_id = ep.f_dia_id
                                                   AND    ep.c_file = ''F''
                                                   AND    ep.c_estado = ''C''
                                                   AND	  ff.c_source = ''SAP'' ';
        c_filtro_solicitud CONSTANT VARCHAR2(1000) := 'AND    ff.c_branch_id IN (SELECT s.c_branch_id
        			                                             FROM   gbu_solicitud s
                                                                             WHERE  0=0
                                                                             AND    s.f_dia_id = ff.f_dia_id
                                                                             AND    s.marca_proceso = ''D''
                                                                             AND    s.c_proceso = ''CFS''
                                                                             ) ';
        c_order_by    CONSTANT 	VARCHAR2(30) := 'ORDER BY ff.c_branch_oi_id';
        c_commit      CONSTANT 	NUMBER (2)                               := 50;
        v_count_commit         	NUMBER (3)                               := 0;
        --v_regauxfact            cur_fact%ROWTYPE;
        v_regauxfact            t_rec_cadena_sap;
        v_reg_fact_mill         gbu_ft_facturacion%ROWTYPE;
        v_reg_po               	typ_reg_po;
        v_branch_oi_id_previo	gbu_ft_facturacion.c_branch_oi_id%TYPE;


        v_branch_oi_ant_id 	gbu_ft_facturacion.c_branch_oi_ant_id%TYPE;
        v_po_id 		gbu_ft_facturacion.c_po_id%TYPE;
        v_mill_oi_id 		gbu_ft_facturacion.c_mill_oi_id%TYPE;
        /*@ARO 12/12/2014*/
        V_AUX_MILL_OI_ID GBU_FT_FACTURACION.C_MILL_OI_ID%TYPE;
        /*@ARO 12/12/2014*/
        v_planta_id		gbu_ft_facturacion.c_planta_id%TYPE;

        v_business_id   	ods_business_transactions.business_id%TYPE;

        v_process_date		DATE:= SYSDATE;

        v_cur_fact 		t_ref_cur;
        v_exist			PLS_INTEGER;
        v_sql			VARCHAR2(3000);

--
BEGIN
	-- Carga de Log --
      	--gbu_util_pkg.graba_gbu_log (7,
        --                          'Comienzo de normalización de facturación',
        --                          'CF');
	v_business_id := NULL;
	v_branch_oi_id_previo	:= 'INICIAL';
        --
        BEGIN
        	SELECT	1
                INTO	v_exist
                FROM	gbu_estado_periodo_vig ep,
                	gbu_solicitud s
                WHERE  	0=0
                AND	ep.f_dia_id = s.f_dia_id
                AND	ep.c_file = 'F'
                AND    	ep.c_estado = 'C'
                AND     s.marca_proceso = 'D'
                AND	s.c_proceso = 'CFS'
                AND	ROWNUM = 1;
        EXCEPTION
        	WHEN NO_DATA_FOUND THEN v_exist := 0;
        END;
        --
        IF (v_exist = 1) THEN
        	v_sql := c_cursor || c_filtro_solicitud || c_order_by;
        ELSE
        	v_sql := c_cursor || c_order_by;
        END IF;
        --
      	OPEN v_cur_fact
        FOR  v_sql;
        FETCH v_cur_fact INTO v_regauxfact;
        WHILE (v_cur_fact%FOUND)
        --
        --OPEN cur_fact;
        --FETCH cur_fact INTO v_regauxfact;
        --WHILE (cur_fact%FOUND)
        LOOP
            --
            v_reg_fact_mill := NULL;
            --
        	IF v_regauxfact.c_branch_oi_id <> v_branch_oi_id_previo
                THEN

                        v_branch_oi_id_previo := v_regauxfact.c_branch_oi_id;
                        v_reg_fact_mill := gbu_ods_calculos_pkg.calculos_fn (v_regauxfact.c_branch_oi_id, 'F');
                        v_reg_po := NULL;
                        IF v_reg_fact_mill.selling_po_comm_loc_code IS NOT NULL THEN
                           v_reg_po.selling_po_comm_loc_code := v_reg_fact_mill.selling_po_comm_loc_code;
                           v_reg_po.selling_po_year   	     := v_reg_fact_mill.selling_po_year;
                           v_reg_po.selling_po_month  	     := v_reg_fact_mill.selling_po_month;
                           v_reg_po.selling_po_doc_type_code := v_reg_fact_mill.selling_po_doc_type_code;
                           v_reg_po.selling_po_number 	     := v_reg_fact_mill.selling_po_number;
                           v_reg_po.selling_po_itm_num 	     := v_reg_fact_mill.selling_po_itm_num;
                           v_reg_po.supplier_subject_code    := gbu_get_ssc_fn (v_reg_po);

                           -- si no trajo nada se deja el original
                           IF v_reg_po.supplier_subject_code IS NULL
                           THEN
                           	v_reg_po.supplier_subject_code    := v_regauxfact.supplier_subject_code;
                           END IF;
			ELSE
                           -- si no se calculan nuevos datos se dejan los que estan
	                   v_reg_po.selling_po_comm_loc_code := v_regauxfact.selling_po_comm_loc_code;
                           v_reg_po.selling_po_year   	     := v_regauxfact.selling_po_year;
                           v_reg_po.selling_po_month  	     := v_regauxfact.selling_po_month;
                           v_reg_po.selling_po_doc_type_code := v_regauxfact.selling_po_doc_type_code;
                           v_reg_po.selling_po_number 	     := v_regauxfact.selling_po_number;
                           v_reg_po.selling_po_itm_num 	     := v_regauxfact.selling_po_itm_num;
                           v_reg_po.supplier_subject_code    := v_regauxfact.supplier_subject_code;
                        END IF;

                       --
                        IF (v_regauxfact.c_mill_oi_id IS NULL) THEN
                           v_regauxfact.c_mill_oi_id := v_reg_fact_mill.c_mill_oi_id;
                        END IF;
                        --
                        IF v_regauxfact.c_branch_id NOT IN ('HUK', 'HSA', 'HMX', 'HSG', 'HNG', 'HIN', 'HUS', 'HJI',
                           'HRU', 'HCA', 'HRF')
                           AND v_reg_fact_mill.c_mill_oi_id <> c_undefined_oi_id
                        THEN
                            -- asignamos el mill_oi_id calculado
                            v_mill_oi_id:= v_reg_fact_mill.c_mill_oi_id;
                        ELSE
                            -- dejamos el valor original
                            v_mill_oi_id:= v_regauxfact.c_mill_oi_id;
                            --
                        END IF;

                        /*@ARO 12/12/2014*/
                        V_AUX_MILL_OI_ID:= GET_MILL_AJUST_CONFERMAS_FN(v_regauxfact.f_dia_id,v_regauxfact.c_branch_oi_id,1,'F');
                        IF V_AUX_MILL_OI_ID <> '000-00000000000000-00000' THEN
                           v_mill_oi_id:= V_AUX_MILL_OI_ID;
                        END IF;
                         /*@ARO 12/12/2014*/

                        -- para el caso de c_branch_oi_ant_id y c_po_id se asigna directamente lo que retorna el calculo
                        v_branch_oi_ant_id:= v_reg_fact_mill.c_branch_oi_ant_id;
                        v_po_id := v_reg_fact_mill.c_po_id;
                        --
                        v_business_id := v_reg_fact_mill.business_id_ref;

                END IF;

                -- calculamos c_planta_id
                v_planta_id := SUBSTR(v_mill_oi_id,1,3);
                --
                --v_business_id := v_reg_fact_mill.business_id_ref;
                --
                -- La tabla solo se actualizará cuando alguno de los campos sea modificado
                IF 	(	v_regauxfact.c_mill_oi_id <> v_mill_oi_id
                	OR	v_regauxfact.c_branch_oi_ant_id <> v_branch_oi_ant_id
                	OR	v_regauxfact.c_po_id <> v_po_id
                        OR	v_regauxfact.supplier_subject_code <> v_reg_po.supplier_subject_code
                        OR	v_regauxfact.selling_po_comm_loc_code <> v_reg_po.selling_po_comm_loc_code
                        OR	v_regauxfact.selling_po_year <> v_reg_po.selling_po_year
                        OR	v_regauxfact.selling_po_month <> v_reg_po.selling_po_month
                        OR	v_regauxfact.selling_po_doc_type_code <> v_reg_po.selling_po_doc_type_code
                        OR	v_regauxfact.selling_po_number <> v_reg_po.selling_po_number
                        OR	v_regauxfact.selling_po_itm_num <> v_reg_po.selling_po_itm_num
                        ) THEN
                        --
                        UPDATE 	gbu_ft_facturacion ff
                        SET    	ff.c_branch_oi_ant_id 		= v_branch_oi_ant_id,
                                ff.c_po_id 			= v_po_id,
                                ff.c_mill_oi_id 		= v_mill_oi_id,
                                ff.c_planta_id			= v_planta_id,
                                ff.supplier_subject_code 	= v_reg_po.supplier_subject_code,
                                ff.selling_po_comm_loc_code	= v_reg_po.selling_po_comm_loc_code ,
                                ff.selling_po_year		= v_reg_po.selling_po_year   	  ,
                                ff.selling_po_month		= v_reg_po.selling_po_month  	  ,
                                ff.selling_po_doc_type_code 	= v_reg_po.selling_po_doc_type_code ,
                                ff.selling_po_number		= v_reg_po.selling_po_number 	  ,
                                ff.selling_po_itm_num		= v_reg_po.selling_po_itm_num 	  ,
                                ff.f_proceso 			= v_process_date,

                                 /*@ARO 08/03/2015*/
                                --ff.c_user_id 			= c_proceso_cadena,
                                ff.c_user_id 			= CASE  WHEN (ff.c_user_id = c_proceso_ajuste)  THEN
                                                          c_proceso_ajuste
                                                    ELSE
                                                          c_proceso_cadena
                                                    END,
                                /*@ARO 08/03/2015*/

                                ff.business_id_ref		= v_business_id
                        WHERE	0=0
                        --AND	ff.rowid = v_regauxfact.rowid;
                        AND	ff.rowid = v_regauxfact.row_id;
                        --
--                        IF (v_exist = 1) THEN
--        			UPDATE 	gbu_solicitud s
--                                SET	s.marca_proceso = 'R'
--                                WHERE	0=0
--                                AND	s.f_dia_id =  v_regauxfact.f_dia_id
--                                AND     s.marca_proceso = 'O'
--                                AND	s.c_proceso = 'CFS';
--                        END IF;
                        --
                END IF;
                --
                v_count_commit := v_count_commit + 1;
                IF v_count_commit = c_commit THEN
                   COMMIT;
                   v_count_commit:=0;
                END IF;
                --
                --FETCH cur_fact INTO v_regauxfact;
                FETCH v_cur_fact INTO v_regauxfact;
                --
        END LOOP;
        --
      	COMMIT;
	--
	--CLOSE cur_fact;
        CLOSE v_cur_fact;

      	-- Carga de Log --
      	--gbu_util_pkg.graba_gbu_log(8,
        --                       	   'Finalización de normalización de facturación',
        --                       	   'CF');

   END calculo_cadena_origen_sap_sp;

--------------------------------------------------------------------------------

PROCEDURE calculo_cadena_origen_man_sp IS

	CURSOR cur_fact IS
        SELECT 	ff.c_branch_id,
                ff.c_branch_oi_id,
                ff.c_branch_oi_ant_id,
                ff.c_mill_oi_id,
                ff.c_po_id,
                ff.selling_po_comm_loc_code,
                ff.selling_po_doc_type_code,
                ff.selling_po_itm_num,
                ff.selling_po_month,
                ff.selling_po_number,
                ff.selling_po_year,
                ff.supplier_subject_code,
                ff.f_dia_id,
                ff.c_source,
        	ff.rowid
        FROM	gbu_ft_facturacion ff,
                gbu_estado_periodo_vig ep,
                gbu_manual_process_wrk mp
        WHERE	0=0
        AND	ff.f_dia_id	= ep.f_dia_id
        AND 	ep.c_file	= '1'
        AND 	ep.c_estado	= '3'
        AND 	ff.c_source	= c_manual_source
        AND 	mp.f_dia_id	= ff.f_dia_id
        AND 	mp.c_branch_id	= ff.c_branch_id
        AND 	mp.c_estado	= c_proceso_carga
        --
        AND	mp.f_dia_id	= ep.f_dia_id
        ORDER BY
        	ff.c_branch_oi_id;


        c_commit      CONSTANT 	NUMBER (2) := 50;
        v_count_commit         	NUMBER (3) := 0;
        v_regauxfact            cur_fact%ROWTYPE;
        v_reg_fact_mill		gbu_ft_facturacion%ROWTYPE;
        v_reg_po               	typ_reg_po;
        v_branch_oi_id_previo	gbu_ft_facturacion.c_branch_oi_id%TYPE;

        v_branch_id_previo	gbu_ft_facturacion.c_branch_id%TYPE;

        v_branch_oi_ant_id 	gbu_ft_facturacion.c_branch_oi_ant_id%TYPE;
        v_po_id 		gbu_ft_facturacion.c_po_id%TYPE;
        v_mill_oi_id 		gbu_ft_facturacion.c_mill_oi_id%TYPE;
        v_planta_id		gbu_ft_facturacion.c_planta_id%TYPE;

        v_business_id   ods_business_transactions.business_id%TYPE;

        v_process_date		DATE:= SYSDATE;

BEGIN
	-- Carga de Log --
      	--gbu_util_pkg.graba_gbu_log (7,
        --                          'Comienzo de normalización de facturación',
        --                          'CF');

	v_branch_oi_id_previo	:= 'INICIAL';
        v_branch_id_previo	:= 'INI';
      	OPEN cur_fact;
        FETCH cur_fact INTO v_regauxfact;
        WHILE (cur_fact%FOUND)
        LOOP
        	IF v_regauxfact.c_branch_oi_id <> v_branch_oi_id_previo
                THEN

                        v_branch_oi_id_previo := v_regauxfact.c_branch_oi_id;
                        v_reg_fact_mill := gbu_ods_calculos_pkg.calculos_fn (v_regauxfact.c_branch_oi_id, 'F');
                        v_reg_po := NULL;
                        IF v_reg_fact_mill.selling_po_comm_loc_code IS NOT NULL
                        THEN
                                v_reg_po.selling_po_comm_loc_code    := v_reg_fact_mill.selling_po_comm_loc_code;
                                v_reg_po.selling_po_year   	     := v_reg_fact_mill.selling_po_year;
                                v_reg_po.selling_po_month  	     := v_reg_fact_mill.selling_po_month;
                                v_reg_po.selling_po_doc_type_code    := v_reg_fact_mill.selling_po_doc_type_code;
                                v_reg_po.selling_po_number 	     := v_reg_fact_mill.selling_po_number;
                                v_reg_po.selling_po_itm_num 	     := v_reg_fact_mill.selling_po_itm_num;
                                v_reg_po.supplier_subject_code       := gbu_get_ssc_fn (v_reg_po);

                                -- si no trajo nada se deja el original
                                IF v_reg_po.supplier_subject_code IS NULL
                                THEN
                                        v_reg_po.supplier_subject_code    := v_regauxfact.supplier_subject_code;
                                END IF;
			ELSE
                                -- si no se calculan nuevos datos se dejan los que estan
                                v_reg_po.selling_po_comm_loc_code := v_regauxfact.selling_po_comm_loc_code;
                                v_reg_po.selling_po_year   	     := v_regauxfact.selling_po_year;
                                v_reg_po.selling_po_month  	     := v_regauxfact.selling_po_month;
                                v_reg_po.selling_po_doc_type_code := v_regauxfact.selling_po_doc_type_code;
                                v_reg_po.selling_po_number 	     := v_regauxfact.selling_po_number;
                                v_reg_po.selling_po_itm_num 	     := v_regauxfact.selling_po_itm_num;
                                v_reg_po.supplier_subject_code    := v_regauxfact.supplier_subject_code;
                        END IF;

                       --
                        IF (v_regauxfact.c_mill_oi_id IS NULL)
                        THEN
				v_regauxfact.c_mill_oi_id := v_reg_fact_mill.c_mill_oi_id;
                        END IF;
                        --
                        IF v_regauxfact.c_branch_id NOT IN ('HUK', 'HSA', 'HMX', 'HSG', 'HNG', 'HIN', 'HUS', 'HJI','HRU', 'HCA', 'HRF')
                           AND v_reg_fact_mill.c_mill_oi_id <> c_undefined_oi_id
                        THEN
                        	-- asignamos el mill_oi_id calculado
				v_mill_oi_id:= v_reg_fact_mill.c_mill_oi_id;
                        ELSE
				-- dejamos el valor original
				v_mill_oi_id:= v_regauxfact.c_mill_oi_id;
                        END IF;

                        -- para el caso de c_branch_oi_ant_id y c_po_id se asigna directamente lo que retorna el calculo
                        v_branch_oi_ant_id:= v_reg_fact_mill.c_branch_oi_ant_id;
                        v_po_id := v_reg_fact_mill.c_po_id;
                        --
                        v_business_id := v_reg_fact_mill.business_id_ref;
			--
                END IF;

                -- calculamos c_planta_id
                v_planta_id := SUBSTR(v_mill_oi_id,1,3);
                --
                --v_business_id := v_reg_fact_mill.business_id_ref;
                --
                -- La tabla solo se actualizará cuando alguno de los campos sea modificado
                IF 	(	v_regauxfact.c_mill_oi_id <> v_mill_oi_id
                	OR	v_regauxfact.c_branch_oi_ant_id <> v_branch_oi_ant_id
                	OR	v_regauxfact.c_po_id <> v_po_id
                        OR	v_regauxfact.supplier_subject_code <> v_reg_po.supplier_subject_code
                        OR	v_regauxfact.selling_po_comm_loc_code <> v_reg_po.selling_po_comm_loc_code
                        OR	v_regauxfact.selling_po_year <> v_reg_po.selling_po_year
                        OR	v_regauxfact.selling_po_month <> v_reg_po.selling_po_month
                        OR	v_regauxfact.selling_po_doc_type_code <> v_reg_po.selling_po_doc_type_code
                        OR	v_regauxfact.selling_po_number <> v_reg_po.selling_po_number
                        OR	v_regauxfact.selling_po_itm_num <> v_reg_po.selling_po_itm_num
                        ) THEN
                        --
                        UPDATE 	gbu_ft_facturacion ff
                        SET    	ff.c_branch_oi_ant_id 		= v_branch_oi_ant_id,
                                ff.c_po_id 			= v_po_id,
                                ff.c_mill_oi_id 		= v_mill_oi_id,
                                ff.c_planta_id			= v_planta_id,
                                ff.supplier_subject_code 	= v_reg_po.supplier_subject_code,
                                ff.selling_po_comm_loc_code	= v_reg_po.selling_po_comm_loc_code ,
                                ff.selling_po_year		= v_reg_po.selling_po_year   	  ,
                                ff.selling_po_month		= v_reg_po.selling_po_month  	  ,
                                ff.selling_po_doc_type_code 	= v_reg_po.selling_po_doc_type_code ,
                                ff.selling_po_number		= v_reg_po.selling_po_number 	  ,
                                ff.selling_po_itm_num		= v_reg_po.selling_po_itm_num 	  ,
                                ff.f_proceso 			= v_process_date,
                                ff.c_user_id 			= c_proceso_cadena,
                                --ff.c_lote_id            = v_business_id
                                ff.business_id_ref		= v_business_id
                        WHERE	0=0
                        AND	ff.rowid = v_regauxfact.rowid;
                        --
                END IF;
                --
                IF (v_regauxfact.c_branch_id <> v_branch_id_previo) THEN
                	--
                        v_branch_id_previo := v_regauxfact.c_branch_id;
                        UPDATE  gbu_manual_process_wrk mp
                        SET	mp.c_estado = c_proceso_cadena,
                                mp.last_updated_date = v_process_date,
                                mp.last_updated_by = c_last_updated_by
                        WHERE	0=0
                        AND	mp.f_dia_id = v_regauxfact.f_dia_id
                        AND	mp.c_branch_id = v_regauxfact.c_branch_id
                        AND	mp.c_estado = c_proceso_carga;
                        --
                END IF;
                --
                v_count_commit := v_count_commit + 1;
                IF v_count_commit = c_commit THEN
                        COMMIT;
                        v_count_commit:=0;
                END IF;
                --
                FETCH cur_fact INTO v_regauxfact;
                --
        END LOOP;
        --
      	COMMIT;
	--
	CLOSE cur_fact;

      	-- Carga de Log --
      	--gbu_util_pkg.graba_gbu_log(8,
        --                       	   'Finalización de normalización de facturación',
        --                       	   'CF');

END calculo_cadena_origen_man_sp;

--------------------------------------------------------------------------------

PROCEDURE calculo_cadena_a_pedido_sp IS
/* ---------------------------------------------------------- */
/* Declaraciones de constantes, variables y cursor generales  */
/* ---------------------------------------------------------- */
        CURSOR v_cur_fact IS /*No se utiliza la gbu_solicitud ya que a PEDIDO no existen Solicitudes*/
        SELECT ff.c_branch_id,
               ff.c_branch_oi_id,
               ff.c_branch_oi_ant_id,
               ff.c_mill_oi_id,
               ff.c_po_id,
               ff.selling_po_comm_loc_code,
               ff.selling_po_doc_type_code,
               ff.selling_po_itm_num,
               ff.selling_po_month,
               ff.selling_po_number,
               ff.selling_po_year,
               ff.supplier_subject_code,
               ff.f_dia_id,
               ff.rowid
        FROM   gbu_ft_facturacion ff,
               gbu_estado_periodo_vig ep
        WHERE  0=0
        AND    ff.f_dia_id = ep.f_dia_id
        AND    ep.c_file   = 'F'
        AND    ep.c_estado = 'C'
        AND	   ff.c_source = 'SAP'
        AND    EXISTS (/*Se realiza este exists ya que pueden haber
                       C_USER_ID igual a AJU_COST (que prevalecen) correspondiente a una Brach levantada a PEDIDO*/
                       SELECT 1
                       FROM gbu_ft_facturacion f1
                       WHERE f1.f_dia_id       = ff.f_dia_id
                       AND   f1.c_source       = ff.c_source
                       AND   f1.c_branch_oi_id = ff.c_branch_oi_id
                       AND   f1.c_user_id      = c_proceso_pedido
                       )
        ORDER BY ff.c_branch_oi_id;

        c_commit      CONSTANT 	NUMBER (2) := 50;
        v_count_commit         	NUMBER (3) := 0;
        v_process_date		      DATE       := SYSDATE;

        v_regauxfact            t_rec_cadena_sap;
        v_reg_po               	typ_reg_po;

        v_reg_fact_mill         gbu_ft_facturacion%ROWTYPE;
        v_branch_oi_id_previo	  gbu_ft_facturacion.c_branch_oi_id%TYPE;
        v_branch_oi_ant_id 	    gbu_ft_facturacion.c_branch_oi_ant_id%TYPE;
        v_po_id 		            gbu_ft_facturacion.c_po_id%TYPE;
        v_planta_id		          gbu_ft_facturacion.c_planta_id%TYPE;
        v_mill_oi_id 		        gbu_ft_facturacion.c_mill_oi_id%TYPE;
        V_AUX_MILL_OI_ID        GBU_FT_FACTURACION.C_MILL_OI_ID%TYPE;

        v_business_id   	      ods_business_transactions.business_id%TYPE;
BEGIN
     v_business_id              := NULL;
	   v_branch_oi_id_previo	    := 'INICIAL';

     FOR v_regauxfact IN v_cur_fact
     LOOP

         v_reg_fact_mill := NULL;

         IF v_regauxfact.c_branch_oi_id <> v_branch_oi_id_previo THEN
                v_branch_oi_id_previo := v_regauxfact.c_branch_oi_id;
                v_reg_fact_mill       := gbu_ods_calculos_pkg.calculos_fn (v_regauxfact.c_branch_oi_id, 'F');
                v_reg_po              := NULL;

                IF v_reg_fact_mill.selling_po_comm_loc_code IS NOT NULL THEN
                       v_reg_po.selling_po_comm_loc_code := v_reg_fact_mill.selling_po_comm_loc_code;
                       v_reg_po.selling_po_year   	     := v_reg_fact_mill.selling_po_year;
                       v_reg_po.selling_po_month  	     := v_reg_fact_mill.selling_po_month;
                       v_reg_po.selling_po_doc_type_code := v_reg_fact_mill.selling_po_doc_type_code;
                       v_reg_po.selling_po_number 	     := v_reg_fact_mill.selling_po_number;
                       v_reg_po.selling_po_itm_num 	     := v_reg_fact_mill.selling_po_itm_num;
                       v_reg_po.supplier_subject_code    := gbu_get_ssc_fn (v_reg_po);

                       -- si no trajo nada se deja el original
                       IF v_reg_po.supplier_subject_code IS NULL THEN
                          v_reg_po.supplier_subject_code := v_regauxfact.supplier_subject_code;
                       END IF;
                ELSE
                       -- si no se calculan nuevos datos se dejan los que estan
                       v_reg_po.selling_po_comm_loc_code := v_regauxfact.selling_po_comm_loc_code;
                       v_reg_po.selling_po_year   	     := v_regauxfact.selling_po_year;
                       v_reg_po.selling_po_month  	     := v_regauxfact.selling_po_month;
                       v_reg_po.selling_po_doc_type_code := v_regauxfact.selling_po_doc_type_code;
                       v_reg_po.selling_po_number 	     := v_regauxfact.selling_po_number;
                       v_reg_po.selling_po_itm_num 	     := v_regauxfact.selling_po_itm_num;
                       v_reg_po.supplier_subject_code    := v_regauxfact.supplier_subject_code;
                END IF;

                IF (v_regauxfact.c_mill_oi_id IS NULL) THEN
                       v_regauxfact.c_mill_oi_id := v_reg_fact_mill.c_mill_oi_id;
                END IF;

                IF v_regauxfact.c_branch_id NOT IN ('HUK','HSA','HMX','HSG','HNG','HIN','HUS','HJI','HRU','HCA','HRF')
                AND v_reg_fact_mill.c_mill_oi_id <> c_undefined_oi_id THEN
                       -- asignamos el mill_oi_id calculado
                       v_mill_oi_id:= v_reg_fact_mill.c_mill_oi_id;
                ELSE
                       -- dejamos el valor original
                       v_mill_oi_id:= v_regauxfact.c_mill_oi_id;
                END IF;

                /*Para Ajustes de Conferma prevalece el ajuste de la conferma*/
                V_AUX_MILL_OI_ID:= GET_MILL_AJUST_CONFERMAS_FN(v_regauxfact.f_dia_id,v_regauxfact.c_branch_oi_id,1,'F');

                IF V_AUX_MILL_OI_ID <> '000-00000000000000-00000' THEN
                       v_mill_oi_id:= V_AUX_MILL_OI_ID;
                END IF;

                -- para el caso de c_branch_oi_ant_id y c_po_id se asigna directamente lo que retorna el calculo
                v_branch_oi_ant_id   := v_reg_fact_mill.c_branch_oi_ant_id;
                v_po_id              := v_reg_fact_mill.c_po_id;
                v_business_id        := v_reg_fact_mill.business_id_ref;

         END IF;

         -- calculamos c_planta_id
         v_planta_id := SUBSTR(v_mill_oi_id,1,3);

         -- La tabla solo se actualizará cuando alguno de los campos sea modificado
         IF 	(	v_regauxfact.c_mill_oi_id             <> v_mill_oi_id
         OR	    v_regauxfact.c_branch_oi_ant_id       <> v_branch_oi_ant_id
         OR	    v_regauxfact.c_po_id                  <> v_po_id
         OR	    v_regauxfact.supplier_subject_code    <> v_reg_po.supplier_subject_code
         OR	    v_regauxfact.selling_po_comm_loc_code <> v_reg_po.selling_po_comm_loc_code
         OR	    v_regauxfact.selling_po_year          <> v_reg_po.selling_po_year
         OR	    v_regauxfact.selling_po_month         <> v_reg_po.selling_po_month
         OR	    v_regauxfact.selling_po_doc_type_code <> v_reg_po.selling_po_doc_type_code
         OR	    v_regauxfact.selling_po_number        <> v_reg_po.selling_po_number
         OR	    v_regauxfact.selling_po_itm_num       <> v_reg_po.selling_po_itm_num ) THEN

                UPDATE 	gbu_ft_facturacion ff
                SET    	ff.c_branch_oi_ant_id 		    = v_branch_oi_ant_id,
                        ff.c_po_id 			              = v_po_id,
                        ff.c_mill_oi_id 		          = v_mill_oi_id,
                        ff.c_planta_id			          = v_planta_id,
                        ff.supplier_subject_code 	    = v_reg_po.supplier_subject_code,
                        ff.selling_po_comm_loc_code	  = v_reg_po.selling_po_comm_loc_code ,
                        ff.selling_po_year		        = v_reg_po.selling_po_year   	  ,
                        ff.selling_po_month	          = v_reg_po.selling_po_month  	  ,
                        ff.selling_po_doc_type_code 	= v_reg_po.selling_po_doc_type_code ,
                        ff.selling_po_number		      = v_reg_po.selling_po_number 	  ,
                        ff.selling_po_itm_num		      = v_reg_po.selling_po_itm_num 	  ,
                        ff.f_proceso 			            = v_process_date,
                        /*@ Si es un ajustes se mantine elusuario de ajuste, para que no se elimne este ajuste en la caga diaria*/
                        ff.c_user_id                  = CASE  WHEN (ff.c_user_id = c_proceso_ajuste)  THEN
                                                              c_proceso_ajuste
                                                        ELSE
                                                              c_proceso_cadena_pedido
                                                        END,
                        ff.business_id_ref            = v_business_id
                        WHERE	0=0
                        AND	ff.rowid = v_regauxfact.rowid;
         END IF;

         v_count_commit := v_count_commit + 1;
         IF v_count_commit = c_commit THEN
            COMMIT;
            v_count_commit:=0;
         END IF;

     END LOOP;

     COMMIT;

END calculo_cadena_a_pedido_sp;

-----------------------------------------------------------------------------------------------

PROCEDURE replica_costos_origen_sap_sp
IS
        CURSOR cur_fact(
        	p_fecha_id 	IN gbu_ft_facturacion.f_dia_id%TYPE,
                p_exist		IN PLS_INTEGER
                )
	IS
        SELECT 	ff.f_dia_id		,
                ff.c_cpto_id               ,
                ff.c_div_id                ,
                ff.c_branch_id             ,
                ff.c_branch_oi_id          ,
                ff.c_planta_id             ,
                ff.c_mill_oi_id            ,
                ff.c_packing_dst_id        ,
                ff.d_product_code          ,
                ff.c_tiptubo_dst_id        ,
                ff.c_diametro_mm_id        ,
                ff.c_end_dst_id            ,
                ff.c_gr_dst_id             ,
                ff.c_norma_dst_id          ,
                ff.d_espesor               ,
                ff.d_libras_pie            ,
                ff.d_longitud_mm           ,
                ff.n_importe               ,
                ff.n_tonnage               ,
                ff.c_source                ,
                ff.c_prod_id               ,
                ff.c_prod_ten_id           ,
                ff.n_meters                ,
                ff.q_quantity              ,
                ff.m_related               ,
                ff.c_file                  ,
                ff.c_tubetype_std_id       ,
                ff.c_costo_mill            ,
                ff.f_proceso               ,
                ff.c_user_id               ,
                ff.n_line                  ,
                ff.n_productividad         ,
                ff.c_rolling_id            ,
                ff.c_protsup_dst_id        ,
                ff.c_protext_dst_id        ,
                ff.c_endcli_dst_id         ,
                ff.c_puerto_dst_id         ,
                ff.c_pais_final_id         ,
                ff.c_cli_fac_id            ,
                ff.c_branch_di_id          ,
                ff.f_doc_date              ,
                ff.c_po_id                 ,
                ff.c_branch_oi_ant_id      ,
                ff.c_branch_oi_orig_id     ,
                ff.c_metodo_costo_mill_id  ,
                ff.c_lote_orig_id          ,
                ff.c_lote_id               ,
                ff.c_moneda                ,
                ff.n_tipo_cambio           ,
                ff.c_process_id            ,
                ff.c_forming_id            ,
                ff.c_flia_p_id             ,
                ff.c_heat_treatment_id     ,
                ff.d_project               ,
                ff.c_sale_channel_branch_id,
                ff.c_sale_channel_bu_id    ,
                ff.c_ctr_trat_term         ,
                ff.c_prod_ctr_trat_term    ,
                ff.n_importe_eur           ,
                ff.service_equiv_inv_tn_qty,
                ff.supplier_subject_code   ,
                ff.service_facility_code   ,
                ff.service_facility_line_code,
                ff.sfso_comm_loc_code      ,
                ff.sfso_year               ,
                ff.sfso_month              ,
                ff.sfso_doc_type_code      ,
                ff.sfso_number             ,
                ff.sfso_itm_num            ,
                ff.selling_po_comm_loc_code,
                ff.selling_po_year         ,
                ff.selling_po_month        ,
                ff.selling_po_doc_type_code,
                ff.selling_po_number       ,
                ff.selling_po_itm_num      ,
                ff.svo_comm_loc_code       ,
                ff.svo_year                ,
                ff.svo_month               ,
                ff.svo_doc_type_code       ,
                ff.svo_number              ,
                ff.svo_itm_num             ,
                ff.svo_type                ,
                ff.sv_so_comm_loc_code_prv ,
                ff.sv_so_year_prv          ,
                ff.sv_so_month_prv         ,
                ff.sv_so_doc_type_code_prv ,
                ff.sv_so_number_prv        ,
                ff.sv_so_itm_num_prv	   ,
                ff.n_real_tonnage	   ,
                ff.business_id_ref	   ,
                ff.sap_company_code
        FROM   	gbu_ft_facturacion ff
        WHERE  	ff.f_dia_id = p_fecha_id
        AND	ff.c_source = c_sap_source
        AND	ff.c_cpto_id IN (cptoCostoSTD, cptoCostoSTD_tercero)
        --
        AND    (1 = p_exist AND ff.c_branch_id IN (SELECT s.c_branch_id
                                                   FROM   gbu_solicitud s
                                                   WHERE  0=0
                                                   AND    s.f_dia_id = ff.f_dia_id
                                                   AND    s.marca_proceso = 'D'
                                                   AND    s.c_proceso = 'CFS'
                                                   )
   	     OR 0 = p_exist)
        --
        ORDER BY ff.c_branch_oi_id;

 	v_process_date		DATE:= SYSDATE;

        v_rec_fact		t_rec_facturacion;
        v_dia_fact		gbu_ft_facturacion.f_dia_id%TYPE;

        v_exist			PLS_INTEGER;

BEGIN
	-- recuperamos la fecha de facturacion para el procesamiento
        BEGIN
        	SELECT	ep.f_dia_id
                INTO	v_dia_fact
                FROM	gbu_estado_periodo_vig ep
                WHERE  	ep.c_file = 'F'
                AND    	ep.c_estado = 'C';
		-- verificamos si hay origenes cargados en la GBU_SOLICITUD para el procesamiento
                BEGIN
                        SELECT	1
                        INTO	v_exist
                        FROM	gbu_solicitud s
                        WHERE  	0=0
                        AND	s.f_dia_id = v_dia_fact
                        AND     s.marca_proceso = 'D'
                        AND	s.c_proceso = 'CFS'
                        AND	ROWNUM = 1;
                EXCEPTION
                        WHEN NO_DATA_FOUND THEN v_exist := 0;
                END;
                --
                -- depuramos los conceptos a generados en un procesamiento anterior
                borrar_conceptos_replica_sp(
                        p_dia_id	=> v_dia_fact,
                        p_source	=> c_sap_source,
                        p_exist		=> v_exist
                        );
                -- por cada registro de facturacion verificamos si se debe o no crear la replica del concepto
                FOR v_rec_fact IN cur_fact(p_fecha_id => v_dia_fact,
                			   p_exist    => v_exist)
                LOOP
                        -- verificamos si hay que replicar
                        -- no existe concepto sap (0060)
                        IF NOT existe_concepto_sesenta_fn(
                                p_branch_oi_id 	=> v_rec_fact.c_branch_oi_id,
                                p_dia_id	=> v_rec_fact.f_dia_id
                                )
                           AND (-- La Branch debe estar Relacionada con la Mill
                                branch_related_mill_fn (
                                        p_branch_id 	=> v_rec_fact.c_branch_id,
                                        p_mill_id 	=> v_rec_fact.c_planta_id
                                        )
                                OR
                                        (
                                        v_rec_fact.c_planta_id IN ('OTH','UND')
                                        AND
                                        v_rec_fact.c_branch_oi_ant_id = c_undefined_oi_id
                                        )
                                OR
                                -- la planta es igual al branch
                                v_rec_fact.c_branch_id = v_rec_fact.c_planta_id
                                )
                        THEN
                                -- replicamos
                                replica_concepto_costo_std_sp(
                                        p_concepto_fact_orig	=> v_rec_fact,
                                        p_process_date		=> v_process_date
                                        );
                                --
--                                IF (v_exist = 1) THEN
--                                        UPDATE 	gbu_solicitud s
--                                        SET	s.marca_proceso = 'P'
--                                        WHERE	0=0
--                                        AND	s.f_dia_id = v_dia_fact
--                                        AND     s.marca_proceso = 'D'
--                                        AND	s.c_proceso = 'CFS';
--                        	END IF;
				--
                        END IF;
                        --
                END LOOP;
		--
                --
                IF (v_exist = 1) THEN
                        UPDATE 	gbu_solicitud s
                        SET	s.marca_proceso = 'P'
                        WHERE	0=0
                        AND	s.f_dia_id = v_dia_fact
                        AND     s.marca_proceso = 'D'
                        AND	s.c_proceso = 'CFS';
                END IF;
                --
                COMMIT;
                --
	EXCEPTION
        	-- si no se encuentra periodo de vigencia valido no se procesa
        	WHEN no_data_found
                THEN
                	NULL;

		-- se se encuentra mas de un periodo de vigencia valido no se procesa
		WHEN too_many_rows
                THEN
                	NULL;
        END;

END replica_costos_origen_sap_sp;

--------------------------------------------------------------------------------

PROCEDURE replica_costos_origen_man_sp
IS
        CURSOR cur_fact(
        	p_fecha_id 	gbu_ft_facturacion.f_dia_id%TYPE
                ) IS
        SELECT 	ff.f_dia_id		,
                ff.c_cpto_id               ,
                ff.c_div_id                ,
                ff.c_branch_id             ,
                ff.c_branch_oi_id          ,
                ff.c_planta_id             ,
                ff.c_mill_oi_id            ,
                ff.c_packing_dst_id        ,
                ff.d_product_code          ,
                ff.c_tiptubo_dst_id        ,
                ff.c_diametro_mm_id        ,
                ff.c_end_dst_id            ,
                ff.c_gr_dst_id             ,
                ff.c_norma_dst_id          ,
                ff.d_espesor               ,
                ff.d_libras_pie            ,
                ff.d_longitud_mm           ,
                ff.n_importe               ,
                ff.n_tonnage               ,
                ff.c_source                ,
                ff.c_prod_id               ,
                ff.c_prod_ten_id           ,
                ff.n_meters                ,
                ff.q_quantity              ,
                ff.m_related               ,
                ff.c_file                  ,
                ff.c_tubetype_std_id       ,
                ff.c_costo_mill            ,
                ff.f_proceso               ,
                ff.c_user_id               ,
                ff.n_line                  ,
                ff.n_productividad         ,
                ff.c_rolling_id            ,
                ff.c_protsup_dst_id        ,
                ff.c_protext_dst_id        ,
                ff.c_endcli_dst_id         ,
                ff.c_puerto_dst_id         ,
                ff.c_pais_final_id         ,
                ff.c_cli_fac_id            ,
                ff.c_branch_di_id          ,
                ff.f_doc_date              ,
                ff.c_po_id                 ,
                ff.c_branch_oi_ant_id      ,
                ff.c_branch_oi_orig_id     ,
                ff.c_metodo_costo_mill_id  ,
                ff.c_lote_orig_id          ,
                ff.c_lote_id               ,
                ff.c_moneda                ,
                ff.n_tipo_cambio           ,
                ff.c_process_id            ,
                ff.c_forming_id            ,
                ff.c_flia_p_id             ,
                ff.c_heat_treatment_id     ,
                ff.d_project               ,
                ff.c_sale_channel_branch_id,
                ff.c_sale_channel_bu_id    ,
                ff.c_ctr_trat_term         ,
                ff.c_prod_ctr_trat_term    ,
                ff.n_importe_eur           ,
                ff.service_equiv_inv_tn_qty,
                ff.supplier_subject_code   ,
                ff.service_facility_code   ,
                ff.service_facility_line_code,
                ff.sfso_comm_loc_code      ,
                ff.sfso_year               ,
                ff.sfso_month              ,
                ff.sfso_doc_type_code      ,
                ff.sfso_number             ,
                ff.sfso_itm_num            ,
                ff.selling_po_comm_loc_code,
                ff.selling_po_year         ,
                ff.selling_po_month        ,
                ff.selling_po_doc_type_code,
                ff.selling_po_number       ,
                ff.selling_po_itm_num      ,
                ff.svo_comm_loc_code       ,
                ff.svo_year                ,
                ff.svo_month               ,
                ff.svo_doc_type_code       ,
                ff.svo_number              ,
                ff.svo_itm_num             ,
                ff.svo_type                ,
                ff.sv_so_comm_loc_code_prv ,
                ff.sv_so_year_prv          ,
                ff.sv_so_month_prv         ,
                ff.sv_so_doc_type_code_prv ,
                ff.sv_so_number_prv        ,
                ff.sv_so_itm_num_prv	   ,
                ff.n_real_tonnage	   ,
                ff.business_id_ref	   ,
                ff.sap_company_code
        FROM   	gbu_ft_facturacion ff,
                gbu_estado_periodo_vig ep,
                gbu_manual_process_wrk mp
        WHERE  	0=0
        AND	ff.f_dia_id	= ep.f_dia_id
        AND 	ep.c_file	= '1'
        AND 	ep.c_estado	= '3'
        AND 	ff.c_source	= c_manual_source
        AND 	mp.f_dia_id	= ff.f_dia_id
        AND 	mp.c_branch_id	= ff.c_branch_id
        AND 	mp.c_estado	= c_proceso_cadena
        AND	ff.f_dia_id 	= p_fecha_id
        AND	ff.c_cpto_id IN (cptoCostoSTD, cptoCostoSTD_tercero)
        ORDER BY ff.c_branch_oi_id;

        c_commit      CONSTANT 	NUMBER (2) := 50;
        v_count_commit         	NUMBER (3) := 0;


 	v_process_date		DATE:= SYSDATE;

        v_rec_fact	t_rec_facturacion;
        v_dia_fact	gbu_ft_facturacion.f_dia_id%TYPE;

BEGIN
	-- recuperamos la fecha de facturacion para el procesamiento
        BEGIN
        	SELECT 	ep.f_dia_id
                INTO	v_dia_fact
                FROM   	gbu_estado_periodo_vig ep
                WHERE  	0=0
                AND	ep.c_file = '1'
                AND    	ep.c_estado = '3'
                AND EXISTS (SELECT  1
                            FROM    gbu_manual_process_wrk mp
                            WHERE   0=0
                            AND	    ep.f_dia_id = mp.f_dia_id
                            AND	    mp.c_estado = c_proceso_cadena
                            );

                -- depuramos los conceptos a generados en un procesamiento anterior
                borrar_conceptos_replica_sp(
                        p_dia_id	=> v_dia_fact,
                        p_source	=> c_manual_source
                        );

                -- por cada registro de facturacion verificamos si se debe o no crear la replica del concepto
                FOR v_rec_fact IN cur_fact(p_fecha_id => v_dia_fact)
                LOOP
                        -- verificamos si hay que replicar
                        -- no existe concepto sap (0060)
                        IF NOT existe_concepto_sesenta_fn(
                                p_branch_oi_id 	=> v_rec_fact.c_branch_oi_id,
                                p_dia_id	=> v_rec_fact.f_dia_id
                                )
                           AND (-- La Branch debe estar Relacionada con la Mill
                                branch_related_mill_fn (
                                        p_branch_id 	=> v_rec_fact.c_branch_id,
                                        p_mill_id 	=> v_rec_fact.c_planta_id
                                        )
                                OR
                                        (
                                        v_rec_fact.c_planta_id IN ('OTH','UND')
                                        AND
                                        v_rec_fact.c_branch_oi_ant_id = c_undefined_oi_id
                                        )
                                OR
                                -- la planta es igual al branch
                                v_rec_fact.c_branch_id = v_rec_fact.c_planta_id
                                )
                        THEN
                                -- replicamos
                                replica_concepto_costo_std_sp(
                                        p_concepto_fact_orig	=> v_rec_fact,
                                        p_process_date		=> v_process_date
                                        );
                                --
                                v_count_commit := v_count_commit + 1;
                                IF v_count_commit = c_commit THEN
                                        COMMIT;
                                        v_count_commit:=0;
                                END IF;

                        END IF;
                END LOOP;
                --
                UPDATE  gbu_manual_process_wrk mp
                SET	mp.c_estado = c_proceso_replica,
                	mp.last_updated_date = v_process_date,
                        mp.last_updated_by = c_last_updated_by
                WHERE	0=0
                AND	mp.f_dia_id = v_dia_fact
                AND	mp.c_estado = c_proceso_cadena;
                --
                COMMIT;
                --
	EXCEPTION
        	-- si no se encuentra periodo de vigencia valido no se procesa
        	WHEN NO_DATA_FOUND
                THEN
                	NULL;

		-- se se encuentra mas de un periodo de vigencia valido no se procesa
		WHEN TOO_MANY_ROWS
                THEN
                	NULL;
        END;

END replica_costos_origen_man_sp;

-----------------------------------------------------------------------------------------------

PROCEDURE replica_costos_a_pedido_sp
IS
        CURSOR cur_fact(
        	p_fecha_id 	IN gbu_ft_facturacion.f_dia_id%TYPE
                )
	IS /*No se utiliza la gbu_solicitud ya que a PEDIDO no existen Solicitudes*/
        SELECT 	ff.f_dia_id		,
                ff.c_cpto_id               ,
                ff.c_div_id                ,
                ff.c_branch_id             ,
                ff.c_branch_oi_id          ,
                ff.c_planta_id             ,
                ff.c_mill_oi_id            ,
                ff.c_packing_dst_id        ,
                ff.d_product_code          ,
                ff.c_tiptubo_dst_id        ,
                ff.c_diametro_mm_id        ,
                ff.c_end_dst_id            ,
                ff.c_gr_dst_id             ,
                ff.c_norma_dst_id          ,
                ff.d_espesor               ,
                ff.d_libras_pie            ,
                ff.d_longitud_mm           ,
                ff.n_importe               ,
                ff.n_tonnage               ,
                ff.c_source                ,
                ff.c_prod_id               ,
                ff.c_prod_ten_id           ,
                ff.n_meters                ,
                ff.q_quantity              ,
                ff.m_related               ,
                ff.c_file                  ,
                ff.c_tubetype_std_id       ,
                ff.c_costo_mill            ,
                ff.f_proceso               ,
                ff.c_user_id               ,
                ff.n_line                  ,
                ff.n_productividad         ,
                ff.c_rolling_id            ,
                ff.c_protsup_dst_id        ,
                ff.c_protext_dst_id        ,
                ff.c_endcli_dst_id         ,
                ff.c_puerto_dst_id         ,
                ff.c_pais_final_id         ,
                ff.c_cli_fac_id            ,
                ff.c_branch_di_id          ,
                ff.f_doc_date              ,
                ff.c_po_id                 ,
                ff.c_branch_oi_ant_id      ,
                ff.c_branch_oi_orig_id     ,
                ff.c_metodo_costo_mill_id  ,
                ff.c_lote_orig_id          ,
                ff.c_lote_id               ,
                ff.c_moneda                ,
                ff.n_tipo_cambio           ,
                ff.c_process_id            ,
                ff.c_forming_id            ,
                ff.c_flia_p_id             ,
                ff.c_heat_treatment_id     ,
                ff.d_project               ,
                ff.c_sale_channel_branch_id,
                ff.c_sale_channel_bu_id    ,
                ff.c_ctr_trat_term         ,
                ff.c_prod_ctr_trat_term    ,
                ff.n_importe_eur           ,
                ff.service_equiv_inv_tn_qty,
                ff.supplier_subject_code   ,
                ff.service_facility_code   ,
                ff.service_facility_line_code,
                ff.sfso_comm_loc_code      ,
                ff.sfso_year               ,
                ff.sfso_month              ,
                ff.sfso_doc_type_code      ,
                ff.sfso_number             ,
                ff.sfso_itm_num            ,
                ff.selling_po_comm_loc_code,
                ff.selling_po_year         ,
                ff.selling_po_month        ,
                ff.selling_po_doc_type_code,
                ff.selling_po_number       ,
                ff.selling_po_itm_num      ,
                ff.svo_comm_loc_code       ,
                ff.svo_year                ,
                ff.svo_month               ,
                ff.svo_doc_type_code       ,
                ff.svo_number              ,
                ff.svo_itm_num             ,
                ff.svo_type                ,
                ff.sv_so_comm_loc_code_prv ,
                ff.sv_so_year_prv          ,
                ff.sv_so_month_prv         ,
                ff.sv_so_doc_type_code_prv ,
                ff.sv_so_number_prv        ,
                ff.sv_so_itm_num_prv	   ,
                ff.n_real_tonnage	   ,
                ff.business_id_ref	   ,
                ff.sap_company_code
        FROM   	 gbu_ft_facturacion ff
        WHERE  	 ff.f_dia_id = p_fecha_id
        AND	     ff.c_source = c_sap_source
        AND	     ff.c_cpto_id IN (cptoCostoSTD, cptoCostoSTD_tercero)
        AND      ff.c_user_id IN (c_proceso_pedido,c_proceso_cadena_pedido)
        ORDER BY ff.c_branch_oi_id;

 	      v_process_date		DATE:= SYSDATE;

        v_rec_fact		t_rec_facturacion;
        v_dia_fact		gbu_ft_facturacion.f_dia_id%TYPE;


BEGIN
	-- recuperamos la fecha de facturacion para el procesamiento
        BEGIN
        	SELECT	ep.f_dia_id
                INTO	v_dia_fact
                FROM	gbu_estado_periodo_vig ep
                WHERE  	ep.c_file = 'F'
                AND    	ep.c_estado = 'C';

                /*No borramos conceptos de replica porque no tendria que eliminar ninguna replica
                ya que en esta instancia no hay replicas */

                -- por cada registro de facturacion verificamos si se debe o no crear la replica del concepto
                FOR v_rec_fact IN cur_fact(p_fecha_id => v_dia_fact)
                LOOP
                        -- verificamos si hay que replicar
                        -- no existe concepto sap (0060)
                        IF NOT existe_concepto_sesenta_fn(
                                p_branch_oi_id 	=> v_rec_fact.c_branch_oi_id,
                                p_dia_id	      => v_rec_fact.f_dia_id
                                )
                           AND (-- La Branch debe estar Relacionada con la Mill
                                branch_related_mill_fn (
                                        p_branch_id 	=> v_rec_fact.c_branch_id,
                                        p_mill_id 	  => v_rec_fact.c_planta_id
                                        )
                                OR
                                        (
                                        v_rec_fact.c_planta_id IN ('OTH','UND')
                                        AND
                                        v_rec_fact.c_branch_oi_ant_id = c_undefined_oi_id
                                        )
                                OR
                                -- la planta es igual al branch
                                v_rec_fact.c_branch_id = v_rec_fact.c_planta_id
                                )
                        THEN
                                -- replicamos
                                replica_concepto_costo_std_sp(
                                        p_concepto_fact_orig	=> v_rec_fact,
                                        p_process_date		=> v_process_date
                                        );

                        END IF;
                END LOOP;

                /*Cambia el C_User ID de Replica y a Pedido*/
               UPDATE gbu_ft_facturacion ff
               SET    ff.c_user_id 			  = CASE
                                            WHEN (ff.c_user_id = c_proceso_cadena_pedido)  THEN c_proceso_cadena
                                            WHEN (ff.c_user_id = c_proceso_pedido)         THEN NULL
                                            ELSE ff.c_user_id END
                WHERE	 0=0
                AND    ff.c_user_id IN (c_proceso_cadena_pedido,c_proceso_pedido)
                AND    ff.f_dia_id = v_dia_fact;
                COMMIT;
                --
                UPDATE 	gbu_solicitud s
                SET	s.marca_proceso = 'P'
                WHERE	0=0
                AND	s.f_dia_id = v_dia_fact
                AND s.marca_proceso = 'D'
                AND	s.c_proceso = 'CFS';
                COMMIT;

	EXCEPTION
        	-- si no se encuentra periodo de vigencia valido no se procesa
        	WHEN no_data_found
                THEN
                	NULL;

		-- se se encuentra mas de un periodo de vigencia valido no se procesa
		WHEN too_many_rows
                THEN
                	NULL;
        END;

END replica_costos_a_pedido_sp;

END gbu_calculo_fact_pkg;
/
