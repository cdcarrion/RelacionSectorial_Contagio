# RelacionSectorial_Contagio


Te muestro lo que tiene la data, es información agregada.


> dta_bce %>% Hmisc::describe()
. 

 78  Variables      77  Observations
-----------------------------------------------------------------------------------------------
Código 
       n  missing distinct 
      77        0       77 

lowest : 01 02 03 04 05, highest: 73 74 75 76 77
-----------------------------------------------------------------------------------------------
Industria 
       n  missing distinct 
      77        0       77 

lowest : Actividades auxiliares de las actividades de servicios financieros                           Actividades de alojamiento                                                                   Actividades de apoyo a la agricultura, poscosecha y tratamiento de semillas para propagación Actividades de atención de la salud humana y de asistencia social no de mercado              Actividades de atención de la salud humana y de asistencia social privada                   
highest: Servicios de enseñanza pública (no de mercado)                                               Servicios Petroleros - Explotación de otras minas y canteras, y actividades de apoyo         Silvicultura y extracción de madera                                                          Suministro de electricidad, gas, vapor y aire acondicionado                                  Transporte y almacenamiento                                                                 
-----------------------------------------------------------------------------------------------
01 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
       77         0        24     0.655  0.001555 3.631e-06  0.002786 0.0000000 0.0000000 
      .25       .50       .75       .90       .95 
0.0000000 0.0000000 0.0001759 0.0047936 0.0084286 

lowest : 0           7.262e-06   0.000108958 0.000138652 0.000175938
highest: 0.00762102  0.0116591   0.0138654   0.0225719   0.022837   
-----------------------------------------------------------------------------------------------
02 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
       77         0        24     0.655  0.001595 6.003e-06  0.002882 0.000e+00 0.000e+00 
      .25       .50       .75       .90       .95 
0.000e+00 0.000e+00 7.086e-05 3.544e-03 1.138e-02 

lowest : 0           1.20055e-05 3.19528e-05 3.41104e-05 7.08589e-05
highest: 0.0106089   0.0144426   0.0154765   0.0162514   0.028037   
-----------------------------------------------------------------------------------------------
03 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
       77         0        25     0.674   0.00741 9.515e-05   0.01447  0.000000  0.000000 
      .25       .50       .75       .90       .95 
 0.000000  0.000000  0.000879  0.004079  0.009005 

lowest : 0           4.3572e-05  0.0001903   0.000287253 0.000836029
highest: 0.00870564  0.0102006   0.0124636   0.0340206   0.465548   
-----------------------------------------------------------------------------------------------
04 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
       77         0        23     0.636  0.001147         0  0.002125 0.000e+00 0.000e+00 
      .25       .50       .75       .90       .95 
0.000e+00 0.000e+00 3.962e-05 2.760e-03 3.942e-03 

lowest : 0           1.41738e-05 1.59764e-05 3.96211e-05 5.56538e-05
highest: 0.00361325  0.00525496  0.00579551  0.0132128   0.0384973  
-----------------------------------------------------------------------------------------------
05 
       n  missing distinct     Info     Mean  pMedian      Gmd      .05      .10      .25 
      77        0       26    0.692  0.01751 0.001855  0.03174 0.000000 0.000000 0.000000 
     .50      .75      .90      .95 
0.000000 0.006403 0.035072 0.127165 

lowest : 0          0.00127085 0.00352927 0.00371043 0.00509159
highest: 0.126602   0.129419   0.144606   0.197838   0.389532  
-----------------------------------------------------------------------------------------------
06 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
       77         0        26     0.692  0.004358 0.0001293  0.007779 0.0000000 0.0000000 
      .25       .50       .75       .90       .95 
0.0000000 0.0000000 0.0007607 0.0115498 0.0188042 
                                                                                              
Value      0.000 0.001 0.002 0.004 0.005 0.006 0.007 0.008 0.009 0.011 0.012 0.015 0.017 0.018
Frequency     58     1     1     2     1     1     1     1     2     1     1     1     1     1
Proportion 0.753 0.013 0.013 0.026 0.013 0.013 0.013 0.013 0.026 0.013 0.013 0.013 0.013 0.013
                                  
Value      0.020 0.029 0.051 0.097
Frequency      1     1     1     1
Proportion 0.013 0.013 0.013 0.013

For the frequency table, variable is rounded to the nearest 0.001
-----------------------------------------------------------------------------------------------
07 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
       77         0        24     0.655  0.002747 2.693e-06  0.005345 0.000e+00 0.000e+00 
      .25       .50       .75       .90       .95 
0.000e+00 0.000e+00 9.449e-05 1.516e-03 4.080e-03 

lowest : 0           5.38585e-06 9.478e-06   6.33659e-05 9.44918e-05
highest: 0.00384648  0.00501238  0.0052491   0.0242625   0.155723   
-----------------------------------------------------------------------------------------------
08 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
       77         0        26     0.692    0.0107 0.0003317      0.02  0.000000  0.000000 
      .25       .50       .75       .90       .95 
 0.000000  0.000000  0.004227  0.011352  0.038939 

lowest : 0           7.00161e-05 0.000270089 0.000663497 0.000719083
highest: 0.0359529   0.0508809   0.0514163   0.179974    0.35648    
-----------------------------------------------------------------------------------------------
09 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
       77         0        29     0.742  0.003832 0.0002249  0.007088 0.0000000 0.0000000 
      .25       .50       .75       .90       .95 
0.0000000 0.0000000 0.0007587 0.0050711 0.0224858 

lowest : 0           0.000135901 0.000177012 0.000242551 0.000341994
highest: 0.0222846   0.0232906   0.0348327   0.0634609   0.0915987  
-----------------------------------------------------------------------------------------------
10 
       n  missing distinct     Info     Mean  pMedian      Gmd      .05      .10      .25 
      77        0       31    0.773   0.0225 0.001373  0.04301 0.000000 0.000000 0.000000 
     .50      .75      .90      .95 
0.000000 0.003631 0.015997 0.041449 
                                                          
Value       0.00  0.01  0.02  0.03  0.05  0.15  0.34  0.96
Frequency     67     3     2     1     1     1     1     1
Proportion 0.870 0.039 0.026 0.013 0.013 0.013 0.013 0.013

For the frequency table, variable is rounded to the nearest 0.01
-----------------------------------------------------------------------------------------------
11 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
       77         0        25     0.674  0.001647 2.251e-05  0.003044 0.0000000 0.0000000 
      .25       .50       .75       .90       .95 
0.0000000 0.0000000 0.0002731 0.0036364 0.0104425 

lowest : 0           3.32967e-05 4.50244e-05 0.000129055 0.000194176
highest: 0.00979791  0.0130208   0.0137239   0.0217456   0.0398437  




> dta_bce_codigo %>% Hmisc::describe()
. 

 14  Variables      19231  Observations
-----------------------------------------------------------------------------------------------
SECCIÓN_CIIU 
       n  missing distinct 
   19209       22       22 

lowest : 0 A B C D, highest: Q R S T U
-----------------------------------------------------------------------------------------------
DESCRIPCIÓN_SECCIÓN 
       n  missing distinct 
   19209       22       22 

lowest : 0                                                                                                                                                ACTIVIDADES DE ALOJAMIENTO Y DE SERVICIO DE COMIDAS.                                                                                             ACTIVIDADES DE ATENCIÓN DE LA SALUD HUMANA Y DE ASISTENCIA SOCIAL.                                                                               ACTIVIDADES DE LOS HOGARES COMO EMPLEADORES; ACTIVIDADES NO DIFERENCIADAS DE LOS HOGARES COMO PRODUCTORES DE BIENES Y SERVICIOS PARA USO PROPIO. ACTIVIDADES DE ORGANIZACIONES Y ÓRGANOS EXTRATERRITORIALES.                                                                                     
highest: INDUSTRIAS MANUFACTURERAS.                                                                                                                       INFORMACIÓN Y COMUNICACIÓN.                                                                                                                      OTRAS ACTIVIDADES DE SERVICIOS.                                                                                                                  SUMINISTRO DE ELECTRICIDAD, GAS, VAPOR Y AIRE ACONDICIONADO.                                                                                     TRANSPORTE Y ALMACENAMIENTO.                                                                                                                    
-----------------------------------------------------------------------------------------------
DIVISIÓN_CIIU 
       n  missing distinct 
   19209       22       85 

lowest : 0   A01 A02 A03 B06, highest: R93 S94 S96 T97 U99
-----------------------------------------------------------------------------------------------
DESCRIPCIÓN_CIIU 
       n  missing distinct 
   19209       22       85 

lowest : 0                                                                                                      ACTIVIDADES ADMINISTRATIVAS Y DE APOYO DE OFICINA Y OTRAS ACTIVIDADES DE APOYO A LAS EMPRESAS.         ACTIVIDADES AUXILIARES DE LAS ACTIVIDADES DE SERVICIOS FINANCIEROS.                                    ACTIVIDADES CREATIVAS, ARTÍSTICAS Y DE ENTRETENIMIENTO.                                                ACTIVIDADES DE AGENCIAS DE VIAJES, OPERADORES TURÍSTICOS, SERVICIOS DE RESERVAS Y ACTIVIDADES CONEXAS.
highest: SUMINISTRO DE ELECTRICIDAD, GAS, VAPOR Y AIRE ACONDICIONADO.                                           TELECOMUNICACIONES.                                                                                    TRANSPORTE POR VÍA ACUÁTICA.                                                                           TRANSPORTE POR VÍA AÉREA.                                                                              TRANSPORTE POR VÍA TERRESTRE Y POR TUBERÍAS.                                                          
-----------------------------------------------------------------------------------------------
CIIU4_ 4D 
       n  missing distinct 
   19209       22      380 

lowest : 0     A0111 A0112 A0113 A0114, highest: S9602 S9603 S9609 T9700 U9900
-----------------------------------------------------------------------------------------------
Descripcion CIIU 4D 
       n  missing distinct 
   19209       22      380 

lowest : 0                                                                                                            ACTIVIDADES COMBINADAS DE APOYO A INSTALACIONES.                                                             ACTIVIDADES COMBINADAS DE SERVICIOS ADMINISTRATIVOS DE OFICINA.                                              ACTIVIDADES CREATIVAS, ARTÍSTICAS Y DE ENTRETENIMIENTO.                                                      ACTIVIDADES DE AGENCIAS DE COBRO Y AGENCIAS DE CALIFICACIÓN CREDITICIA.                                     
highest: VENTA AL POR MENOR EN COMERCIOS NO ESPECIALIZADOS CON PREDOMINIO DE LA VENTA DE ALIMENTOS, BEBIDAS O TABACO. VENTA AL POR MENOR POR CORREO Y POR INTERNET.                                                                VENTA DE PARTES, PIEZAS Y ACCESORIOS PARA VEHÍCULOS AUTOMOTORES.                                             VENTA DE VEHÍCULOS AUTOMOTORES.                                                                              VENTA, MANTENIMIENTO Y REPARACIÓN DE MOTOCICLETAS Y DE SUS PARTES, PIEZAS Y ACCESORIOS.                     
-----------------------------------------------------------------------------------------------
CIIU4_6D 
       n  missing distinct 
   19209       22     1421 

lowest : 0       A011111 A011112 A011113 A011119, highest: S960907 S960908 T970000 U990001 U990002
-----------------------------------------------------------------------------------------------
DESCRIPCION_CIIU4_6D 
       n  missing distinct 
   19181       50     1419 

lowest : 0                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Actividad de Operadores turísticos que se encargan de la planificación y organización de paquetes de servicios de viajes (tours) para su venta a través de agencias de viajes o por los propios operadores turísticos. Esos viajes organizados (tours) pueden incluir la totalidad o parte de las siguientes características: transporte, alojamiento, comidas, visitas a museos, lugares históricos o culturales, espectáculos teatrales, musicales o deportivos. Actividades a corto y a largo plazo de clínicas del día, básicas y generales, es decir, actividades médicas, de diagnóstico y de tratamiento.                                                                                                                                                                                                                                                                                                                      Actividades a corto y a largo plazo de clínicas especializadas, es decir, actividades médicas, de diagnóstico y de tratamiento (clínicas para enfermos mentales, de rehabilitación, para enfermedades infecciosas, de maternidad, etcétera).                                                                                                                                                                                                                       Actividades a corto y a largo plazo de los hospitales básicos y generales, es decir, actividades médicas, de diagnóstico y de tratamiento (hospitales: comunitarios y regionales, de organi ... <truncated>
highest: Venta de partes, piezas y accesorios para motocicletas (incluso por comisionistas y compañías de venta por correo).                                                                                                                                                                                                                                                                                                                                                Venta de todo tipo de partes, componentes, suministros, herramientas y accesorios para vehículos automotores como: neumáticos (llantas), cámaras de aire para neumáticos (tubos). Incluye bujías, baterías, equipo de iluminación partes y piezas eléctricas.                                                                                                                                                                                                      Venta de vehículos nuevos y usados: vehículos de pasajeros, incluidos vehículos especializados como: ambulancias y minibuses, camiones, remolques y semirremolques, vehículos de acampada como: caravanas y autocaravanas, vehículos para todo terreno (jeeps, etcétera), incluido la venta al por mayor y al por menor por comisionistas.                                                                                                                         Venta directa de combustibles (combustible de calefacción, leña, etcétera) con entrega en el domicilio del cliente.                                                                                                                                                                                                                                                                                                                                                Vuelos panorámicos y turísticos incluye actividades generales de aviación, como: transporte de pasajeros por clubes aéreos con fines de instrucción o de recreo.                            ... <truncated>
-----------------------------------------------------------------------------------------------
CPC2_9D 
       n  missing distinct 
   19231        0    19080 

lowest : 0         011110001 011110002 011120100 011120201
highest: 990000205 990000206 990000207 990000208 990000299
-----------------------------------------------------------------------------------------------
DESCRIPCION_CPC2_9D 
       n  missing distinct 
   19231        0    19022 

lowest :  Servicios de  instalación de luminarias exterior e interior, interruptores y accesorios, etc. (Las actividades de construcción especializada se realizan en su mayoría mediante subcontratación). (D- y DL-) -Pantotenato de calcio                                                                                                                                                                  (D- y DL-) -Pantotenato de sodio                                                                                                                                                                   (Hidroximetil)riboflavina o metilolriboflavina                                                                                                                                                     (L) Ascorbozinconinato de estroncio                                                                                                                                                               
highest: Zotepina                                                                                                                                                                                           Zuclopentixol                                                                                                                                                                                      Zuecos de madera, con palas de cuero o cuero regenerado                                                                                                                                            Zuquini fresco o refrigerado                                                                                                                                                                       Zuquinis congelados, incluso cocidas                                                                                                                                                              
-----------------------------------------------------------------------------------------------
CÓDIGO DE INDUSTRIA ECUATORIANO_CIE 
       n  missing distinct 
   19231        0       75 

lowest : 01 02 03 04 05, highest: 72 73 74 75 76
-----------------------------------------------------------------------------------------------
DESCRIPCIÓN DE CIE 
       n  missing distinct 
   19231        0       75 

lowest : Actividades auxiliares de las actividades de servicios financieros              Actividades de alojamiento                                                      Actividades de atención de la salud humana y de asistencia social no de mercado Actividades de atención de la salud humana y de asistencia social privada       Actividades de los hogares como empleadores de personal doméstico              
highest: Servicios de enseñanza pública (no de mercado)                                  Servicios Petroleros                                                            Silvicultura y extracción de madera                                             Suministro de electricidad, gas, vapor y aire acondicionado                     Transporte y almacenamiento                                                    
-----------------------------------------------------------------------------------------------
CPE Publicación 
       n  missing distinct 
   19231        0      226 

lowest : 001 002 003 004 005, highest: 226 227 228 229 230
-----------------------------------------------------------------------------------------------
CPE Publicación Descripción 
       n  missing distinct 
   19231        0      226 

lowest : Aceites vegetales, crudo                   Aceites vegetales, refinado                Actividades de telecomunicaciones          Agua                                       Alcohol etílico y bebidas alcohólicas     
highest: Trigo                                      Vegetales leguminosos secos                Vehículos automotores, partes y accesorios Vidrio y productos de vidrio               Yuca                                      
-----------------------------------------------------------------------------------------------


> dta_bitacora2026 %>% Hmisc::describe()
. 

 11  Variables      922  Observations
-----------------------------------------------------------------------------------------------
ACTIVIDAD ECONÓMICA 
       n  missing distinct 
     922        0      922 

lowest : 99999|NO DEFINIDO                                                                                                       A011111|CULTIVO DE TRIGO                                                                                                A011112|CULTIVO DE MAIZ DURO                                                                                            A011112|CULTIVO DE MAIZ SUAVE                                                                                           A011113|CULTIVO DE QUINUA                                                                                              
highest: T981000|ACTIVIDADES NO DIFERENCIADAS DE HOGARES COMO PRODUCTORES DE BIENES DE SUBSISTENCIA                              T982000|ESTUDIANTE                                                                                                      T982000|JUBILADO                                                                                                        U990001|ACTIVIDADES DE ORGANIZACIONES INTERNACIONALES COMO LAS NACIONES UNIDAS Y LOS ORGANISMOS ESPECIALIZADOS          U990002|ACTIVIDADES DE MISIONES DIPLOMATICAS Y CONSULARES CUANDO ESTAN DETERMINADAS POR EL PAIS EN EL QUE SE ENCUENTRAN
-----------------------------------------------------------------------------------------------
codigo_actividad 
       n  missing distinct 
     922        0      882 

lowest : 99999   A011111 A011112 A011113 A011119, highest: T970000 T981000 T982000 U990001 U990002
-----------------------------------------------------------------------------------------------
descripcion_actividad 
       n  missing distinct 
     922        0      922 

lowest : ABASTECIMIENTO DE EVENTOS Y OTRAS ACTIVIDADES DE SERVICIO DE COMIDAS                                                ACTIVIDAD DE PROCESAMIENTO DE DESPERDICIO  METALICO Y NO METALICO Y OTRO ARTICULO PARA CONVERTIRLO EN MATERIA PRIMA ACTIVIDAD DE RECOLECCION DE DESECHO PELIGROSO SUSTANCIA EXPLOSIVA OXIDANTE INFLAMABLE TOXICA IRRITANTE CARCINOGENA  ACTIVIDADES A CORTO Y A LARGO PLAZO DE CLINICAS DEL DIA BASICAS Y GENERALES ES DECIR ACTIVIDADES MEDICAS            ACTIVIDADES A CORTO Y A LARGO PLAZO DE LOS HOSPITALES ESPECIALIZADOS ES DECIR ACTIVIDADES MEDICAS DE DIAGNOSTICO   
highest: VENTA DE PARTES, PIEZAS Y ACCESORIOS PARA MOTOCICLETAS INCLUSO POR COMISIONISTAS Y COMPANIAS DE VENTA POR CORREO    VENTA DE TODO TIPO DE PARTES, COMPONENTES, SUMINISTROS, HERRAMIENTAS COMO: NEUMATICOS (LLANTAS)                     VENTA DIRECTA DE COMBUSTIBLES COMBUSTIBLE DE CALEFACCION, LENA, ETCETERA CON ENTREGA EN EL DOMICILIO DEL CLIENTE    VIVERES Y ABARROTES (AL POR MENOR)                                                                                  VIVIENDA (CASAS Y DEPARTAMENTOS)                                                                                   
-----------------------------------------------------------------------------------------------
CLUSTER 
       n  missing distinct 
     922        0       44 

lowest : ARROZ                                    ATUN Y CONSERVAS DE PRODUCTOS DEL MAR    AUTOMOTRIZ                               AVÍCOLA                                  BANANO                                  
highest: SUPER FOOD                               TELECOMUNICACIONES                       TEXTIL                                   TRANSPORTE TERRESTRE (CARGA Y PASAJEROS) TRANSPORTE Y ALMACENAMIENTO             
-----------------------------------------------------------------------------------------------

Variables with all observations missing:

[1] CÓDIGO                    PUNTAJE\r\nDICIEMBRE 2025 PUNTAJE\r\nMARZO 2026    
[4] VARIACIÓN Mar/Dic         Observaciones             Sugerencia Sectorial     
[7] Puntaje Sugerido MAR 2026



> dta_empresas_ventas %>% Hmisc::describe()
. 

 11  Variables      19305  Observations
-----------------------------------------------------------------------------------------------
ANIO FISCAL 
       n  missing distinct     Info     Mean  pMedian      Gmd 
   19305        0        5    0.945     2024     2024    1.344 
                                        
Value       2022  2023  2024  2025  2026
Frequency   4634  4617  4643  4637   774
Proportion 0.240 0.239 0.241 0.240 0.040
-----------------------------------------------------------------------------------------------
MES FISCAL 
       n  missing distinct     Info     Mean  pMedian      Gmd      .05      .10      .25 
   19305        0       12    0.993      6.3      6.5    4.053        1        1        3 
     .50      .75      .90      .95 
       6        9       11       12 
                                                                      
Value         1    2    3    4    5    6    7    8    9   10   11   12
Frequency  1932 1929 1544 1544 1543 1552 1544 1544 1541 1541 1540 1551
Proportion 0.10 0.10 0.08 0.08 0.08 0.08 0.08 0.08 0.08 0.08 0.08 0.08
-----------------------------------------------------------------------------------------------
PERIODO 
       n  missing distinct 
   19305        0       50 

lowest : 20221  202210 202211 202212 20222 , highest: 20257  20258  20259  20261  20262 
-----------------------------------------------------------------------------------------------
ACTIVIDAD ECONOMICA 
       n  missing distinct 
   19305        0      389 

lowest : A011112 A011122 A011132 A011139 A011200, highest: S952401 S952902 S960101 S960200 S960901
-----------------------------------------------------------------------------------------------
ACTIVIDAD CIIU 
       n  missing distinct 
   19305        0      389 

lowest : Actividades a corto y a largo plazo de clínicas del día, básicas y generales, es decir, actividades médicas, de diagnóstico y de tratamiento.                                                                                                                                                                                                                                                                           Actividades a corto y a largo plazo de los hospitales básicos y generales, es decir, actividades médicas, de diagnóstico y de tratamiento (hospitales: comunitarios y regionales, de organizaciones sin fines de lucro, universitarios, de bases militares y de prisiones, del Ministerio de gobierno y policía, del Ministerio de defensa nacional, de la Junta de Beneficencia, del Seguro Social, Fisco Misionales). Actividades a corto y a largo plazo de los hospitales especializados, es decir, actividades médicas, de diagnóstico y de tratamiento (hospitales para enfermos mentales, centros de rehabilitación, hospitales para enfermedades infecciosas, de maternidad, sanatorios especializados, etcétera).                                                                                                                      Actividades de acondicionamiento y mantenimiento de terrenos para usos agrícolas: plantación o siembra de cultivos y cosecha, poda de árboles frutales y viñas, transplante de arroz y entresacado de remolacha.                                                                                                                                                                                                        Actividades de agentes y corredores inmobiliarios. Intermediación en la compra, venta y alquiler de bienes inmuebles a cambio de una retribución o por contrato.                                                                                                                                                                                                        ... <truncated>
highest: Venta de motocicletas, incluso ciclomotores (velomotores), tricimotos.                                                                                                                                                                                                                                                                                                                                                  Venta de partes, piezas y accesorios para motocicletas (incluso por comisionistas y compañías de venta por correo).                                                                                                                                                                                                                                                                                                     Venta de todo tipo de partes, componentes, suministros, herramientas y accesorios para vehículos automotores como: neumáticos (llantas), cámaras de aire para neumáticos (tubos). Incluye bujías, baterías, equipo de iluminación partes y piezas eléctricas.                                                                                                                                                           Venta de vehículos nuevos y usados: vehículos de pasajeros, incluidos vehículos especializados como: ambulancias y minibuses, camiones, remolques y semirremolques, vehículos de acampada como: caravanas y autocaravanas, vehículos para todo terreno (jeeps, etcétera), incluido la venta al por mayor y al por menor por comisionistas.                                                                              Vuelos panorámicos y turísticos incluye actividades generales de aviación, como: transporte de pasajeros por clubes aéreos con fines de instrucción o de recreo.                                                                                                                                                                                                        ... <truncated>
-----------------------------------------------------------------------------------------------
ACTIVIDAD BP 
       n  missing distinct 
   19305        0      389 

lowest : A011112|CULTIVO DE MAIZ DURO                                                                                                 A011122|CULTIVO DE ARVEJA                                                                                                    A011132|CULTIVO DE SEMILLAS DE MANI                                                                                          A011139|OTROS CULTIVOS DE SEMILLAS OLEAGINOSAS SEMILLAS DE RICINO SEMILLAS DE LINAZA SEMILLAS DE MOSTAZA SEMILLAS DE GIRASOL A011200|CULTIVO DE ARROZ (INCLUIDO EL CULTIVO ORGANICO Y EL CULTIVO DE ARROZ GENETICAMENTE MODIFICADO)                      
highest: S952401|RETAPIZADO, REPARACION Y RESTAURACION DE MUEBLES Y ACCESORIOS DOMESTICOS, INCLUIDOS MUEBLES DE OFICINA               S952902|REPARACION Y ARREGLO DE PRENDAS DE VESTIR                                                                            S960101|LAVANDERIA ROPA                                                                                                      S960200|SALON DE BELLEZA Y PELUQUERIA                                                                                        S960901|ACTIVIDADES DE BANOS TURCOS, DE VAPOR, PUBLICOS, SAUNAS, CENTROS DE SPA, SALONES ADELGAZAMIENTO DE MASAJE, ETCETERA 
-----------------------------------------------------------------------------------------------
SECTOR BP 
       n  missing distinct 
   19305        0       39 

lowest : ARROZ                                    ATUN Y CONSERVAS DE PRODUCTOS DEL MAR    AUTOMOTRIZ                               AVÍCOLA                                  BANANO                                  
highest: SUPER FOOD                               TELECOMUNICACIONES                       TEXTIL                                   TRANSPORTE TERRESTRE (CARGA Y PASAJEROS) TRANSPORTE Y ALMACENAMIENTO             
-----------------------------------------------------------------------------------------------
PERSONAS NATURALES/TOTAL VENTAS Y EXPORTACIONES (419) 
       n  missing distinct     Info     Mean  pMedian      Gmd      .05      .10      .25 
   19305        0    18507        1  8117399  2816326 13015676     2025    31004   236469 
     .50      .75      .90      .95 
 1320175  6031421 18740847 37169356 

lowest : 0         0.01      0.02      0.04      0.1      
highest: 541572000 616471000 631764000 733674000 775055000
-----------------------------------------------------------------------------------------------
SOCIEDADES/TOTAL VENTAS Y EXPORTACIONES (419) 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
    19305         0     18467         1  32811167  10888076  52819179      1654     80889 
      .25       .50       .75       .90       .95 
   917531   5270551  23177300  82523711 133790104 

lowest : 0          0.01       0.02       0.11       1         
highest: 1750820000 1785240000 1838120000 1984760000 2036020000
-----------------------------------------------------------------------------------------------
TOTAL VENTAS 
        n   missing  distinct      Info      Mean   pMedian       Gmd       .05       .10 
    19305         0     19140         1  40928567  16080285  63418035     81059    250381 
      .25       .50       .75       .90       .95 
  1991429   8761245  33291609 102616865 175751144 

lowest : 0          0.01       1          1.01       1.25      
highest: 1766920000 1798550000 1852030000 1998770000 2048880000
-----------------------------------------------------------------------------------------------
CRITERIO 
       n  missing distinct    value 
   19305        0        1   CUMPLE 
                 
Value      CUMPLE
Frequency   19305
Proportion      1
-----------------------------------------------------------------------------------------------



Esto fue lo que me salió 

> tasa_contagio %>% glimpse()
Rows: 1,289
Columns: 10
$ CLUSTER_ORIGEN     <chr> "CAMARON Y PRODUCTOS DERIVADOS", "CULTIVOS CICLO CORTO", "AVÍCOLA"…
$ CLUSTER_DESTINO    <chr> "FLORES", "FLORES", "FLORES", "CARTON Y PAPEL", "PLÁSTICOS Y CAUCH…
$ peso               <dbl> 2.038373e-03, 2.137014e-03, 1.772591e-03, 5.409950e-04, 1.659166e-…
$ eventos_origen     <int> 3, 3, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 4, 3, 3, 3, 3, 3, 3, 4, …
$ tasa_mismo_mes     <dbl> 0.0000000, 0.0000000, 0.0000000, 0.3333333, 0.3333333, 0.3333333, …
$ tasa_mes_siguiente <dbl> 0.5000000, 0.3333333, 0.2500000, 0.5000000, 0.5000000, 0.5000000, …
$ tasa_base          <dbl> 0.02777778, 0.02777778, 0.02777778, 0.05555556, 0.05555556, 0.0555…
$ n_meses            <int> 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36…
$ lift_mismo_mes     <dbl> 0.0, 0.0, 0.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 3.0, 7…
$ lift_mes_siguiente <dbl> 18, 12, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 8, 6, 6, 6, 6, 6, 6, 6…
