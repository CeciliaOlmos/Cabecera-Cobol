      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CECILIA-OLMOS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCH-TRANS
           ASSIGN TO "../transacciones.dat"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT TRANSC-ACTUAL
           ASSIGN TO "../transAct.dat"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT LISTADO ASSIGN TO
           PRINTER, "../impre.dat".

       DATA DIVISION.
       FILE SECTION.
       FD  ARCH-TRANS.
       01  tr-cab-reg.
           03 tr-cab-tipo PIC X.
           03 tr-cab-fecha PIC s9(8).
       01  tr-det-reg.
           03 tr-det-tipo PIC X.
           03 tr-det-socio PIC 9999.
           03 tr-det-importe PIC S9(7)V99.

       FD  TRANSC-ACTUAL.
       01  tra-reg.
           03 tra-socio pic 9(4).
           03 tra-importe pic S9(8)V99.

       FD  listado
           LINAGE IS 60 LINES
           with FOOTING AT 50
           lines at top 1
           lines at BOTTOM 1.
       01  lis-reg pic x(80).

       WORKING-STORAGE SECTION.
       01  w-flag-transc pic 9.
       01  w-fecha-ing pic s9(8).
       01  w-socio-ant pic 9(4).

       01  w-imp-procesado pic s9(8)v99.
       01  w-band pic x value "n".
       01  cabecera1.
           03 filler pic x(28).
           03 filler pic x(24) value "LISTADO DE TRANSACCIONES".
           03 filler pic x(28) value spaces.
       01  cabecera2.
           03 filler pic x(80) value all "-".
       01  cabecera3.
           03 filler pic x(17) value spaces.
           03 filler pic x(5) value "SOCIO".
           03 filler pic x(8) value spaces.
           03 filler pic x(7) value "IMPORTE".
           03 filler pic x(20) value spaces.
       01  detalle.
           03 filler pic x(17) value spaces.
           03 l-soc pic x(5) value spaces.
           03 filler pic x(5) value spaces.
           03 l-saldo pic zz.zzz.zz9,99.
           03 filler pic x(20) value spaces.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-LEER-TRANSAC.
            PERFORM UNTIL w-flag-transc is EQUAL 1
                      PERFORM 300-INICIO-FECHA
                      PERFORM 340-DESAGOTAR-DETALLE
                 PERFORM UNTIL w-flag-transc is equal 1
                            or tr-det-tipo is equal "C"
                      PERFORM 350-INICIO-SOCIO
                   PERFORM UNTIL  w-flag-transc is equal 1
                              or tr-det-socio is not equal w-socio-ant
                            PERFORM 400-PROCESO
                            PERFORM 200-LEER-TRANSAC
                   END-PERFORM
                      PERFORM 450-FIN-SOCIO
                   end-perform
                      PERFORM 500-FIN-FECHA
            END-PERFORM.
            PERFORM 600-FIN-GENERAL.
            STOP RUN.

       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVOS.
           PERFORM 130-INICIO-VARIABLES.
           PERFORM 140-INGRESAR-FECHA.
           PERFORM 160-LISTAR-ENCABEZADO.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT ARCH-TRANS.
           OPEN OUTPUT TRANSC-ACTUAL.
           OPEN OUTPUT LISTADO.

       130-INICIO-VARIABLES.
           MOVE ZERO TO w-flag-transc.

       140-INGRESAR-FECHA.
           DISPLAY "Ingrese fecha de transaccion (AAAAMMDD)".
           ACCEPT w-fecha-ing.
           PERFORM until w-fecha-ing is > 0
           DISPLAY "Error, Ingrese fecha de transaccion (AAAAMMDD)"
           ACCEPT w-fecha-ing
           END-PERFORM.

       160-LISTAR-ENCABEZADO.
           WRITE lis-reg FROM cabecera1 AFTER 1.
           WRITE lis-reg FROM cabecera2 AFTER 1.
           WRITE lis-reg FROM cabecera3 AFTER 1.

       200-LEER-TRANSAC.
           READ ARCH-TRANS AT END MOVE 1 TO w-flag-transc.

       300-INICIO-FECHA.
           PERFORM 330-BUSCAR-FECHA.

       330-BUSCAR-FECHA.
           PERFORM 200-LEER-TRANSAC UNTIL w-flag-transc IS EQUAL 1
               OR (tr-cab-fecha IS equal w-fecha-ing
               AND tr-cab-tipo IS EQUAL "C").

       340-DESAGOTAR-DETALLE.
           IF tr-cab-tipo is EQUAL to "C" and
              tr-cab-fecha IS  EQUAL w-fecha-ing
                 PERFORM 200-LEER-TRANSAC
                 MOVE "s" to w-band.

       350-INICIO-SOCIO.
           MOVE tr-det-socio to w-socio-ant.
           MOVE ZERO to w-imp-procesado.

       400-PROCESO.
           ADD tr-det-importe to w-imp-procesado.

       450-FIN-SOCIO.
           PERFORM 470-ARMO-ARCHIVO.
           PERFORM 490-ARMO-IMPRESION.

       470-ARMO-ARCHIVO.
           MOVE w-socio-ant to tra-socio.
           MOVE w-imp-procesado to tra-importe.
           WRITE tra-reg.

       490-ARMO-IMPRESION.
           MOVE w-socio-ant TO l-soc.
           MOVE w-imp-procesado TO l-saldo.
           write lis-reg FROM detalle AFTER 1.
           DISPLAY lis-reg.

       500-FIN-FECHA.
           if w-band is EQUAL "n" THEN
              DISPLAY "No hay transacciones en la fecha ingresada".

       600-FIN-GENERAL.
           CLOSE ARCH-TRANS.
           CLOSE TRANSC-ACTUAL.
           CLOSE LISTADO.

       END PROGRAM CECILIA-OLMOS.
