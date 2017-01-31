CREATE OR REPLACE PACKAGE ODS_COMMON_PKG
IS
/*********************************************************************************************
Author  :   Fidel Nakashima (T52585)
Created :   2011-12-20
Purpose :   Administrar las estructuras comunes a los paquetes ODS

Historial
Date            Person				Description
------------    ------------------	-------------------------------------
2011-12-20	T52585			Creacion del Paquete
------------    ------------------	-------------------------------------
2015-08-26	T53605	(Pedro Dias)	Por cambio de definicion en la logica se modifica el valor de la constante c_one_data_source (antes SAP, hoy ONE)
                                        para que pueda calcular correctamente la rama significativa de un negocio entre otras cosas.

*********************************************************************************************/

-- Public type declarations
	TYPE t_refcursor IS REF CURSOR;

        SUBTYPE s_str_boolean IS VARCHAR2(5);

        SUBTYPE s_str_yes_no IS VARCHAR2(1);

        SUBTYPE s_data_source IS VARCHAR2(10);

	SUBTYPE s_busn_chain_type IS VARCHAR2(1);

	SUBTYPE s_error_msg IS VARCHAR2(250);

        TYPE t_table_business_id IS TABLE OF ods_business_transactions.business_id%TYPE
        INDEX BY BINARY_INTEGER;

-- Public constant declarations
        c_str_true CONSTANT s_str_boolean:= 'TRUE';
        c_str_false CONSTANT s_str_boolean:= 'FALSE';

	c_str_yes CONSTANT s_str_yes_no:= 'Y';
	c_str_no  CONSTANT s_str_yes_no:= 'N';

	c_str_null CONSTANT VARCHAR2(5):= 'NULL';

        --
        --Se cambia el dato de la constante, se reemplaza SAP por ONE. Este cambio en la definicion fue realizado hace tiempo pero el cambio no se aplico en
        --el valor de la constante. Consecuencias no se calculaban correctamente las ramas significativas de un negocio.
        --c_one_data_source CONSTANT s_data_source:= 'SAP';
        c_one_data_source CONSTANT s_data_source:= 'ONE';
        --
        c_ten_data_source CONSTANT s_data_source:= 'TEN';
        c_man_data_source CONSTANT s_data_source:= 'MAN';
        c_sales_data_source CONSTANT s_data_source:= 'SALES';

        c_doc_tree_one_data_source CONSTANT s_data_source:= 'ONE';

        c_default_no_man_data_source CONSTANT s_data_source:= 'ONE';

	c_build_to_stock_type	CONSTANT s_busn_chain_type:= 'S';
	c_build_to_order_type	CONSTANT s_busn_chain_type:= 'B';

        c_invalid_portfolio_type CONSTANT VARCHAR2(2):= '99';

        c_invalid_millack_doc_num CONSTANT ods_business_document_tree.mill_ack_doc_num%TYPE:= '00000000';
        c_metalmecanica_comm_loc CONSTANT ods_business_document_tree.mill_ack_doc_comm_loc_code%TYPE:= 'G17';

        c_invalid_business_id CONSTANT ods_business_transactions.business_id%TYPE:= 9999999999;

	c_error_code_end_of_list	PLS_INTEGER:= -20001;
	c_error_msg_end_of_list		s_error_msg:= 'End of list, no more records to process';
	c_separator			VARCHAR2(1):= '|';

-- Public variable declarations

-- Public Exception

-- Public function and procedure declarations


END ODS_COMMON_PKG;
/
