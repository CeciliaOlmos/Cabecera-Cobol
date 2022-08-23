      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
         ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACCIONES
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\Cabecera\transacciones.dat"
           ORGANIZATION is line sequential.
           SELECT TRANSAC-ACTUAL
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\Cabecera\archTransAct.dat"
           ORGANIZATION is line sequential.
       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACCIONES.
       01  tr-cab-reg.
           03 tr-cab-tipo pic x.
           03 tr-cab-fecha PIC s9(8).
       01  tr-det-reg.
           03 tr-det-tipo pic x.
           03 tr-det-socio pic 9(4).
           03 tr-det-importe pic s9(7)v99.

       FD  TRANSAC-ACTUAL.
       01  tra-reg.
           03 tra-socio pic 9(4).
           03 tra-importe pic s9(8)v99.

       WORKING-STORAGE SECTION.
       01  w-flag-transc pic 9 VALUE ZERO.
       01  w-fecha-ing pic s9(8).
       01  w-socio-ant pic 9(4).
       01  w-imp-procesado pic s9(8)v99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-LEER-ARCHIVO.
           PERFORM 300-PEDIR-FECHA.
           PERFORM UNTIL w-flag-transc IS EQUAL 1
               PERFORM 400-INICIO-FECHA
      *         PERFORM 500-INICIO-SOCIO
      *         PERFORM UNTIL w-flag-transc IS EQUAL 1
      *             OR tr-det-socio IS NOT EQUAL w-socio-ant
      *                 PERFORM 600-PROCESO-SOCIO
      *                 PERFORM 200-LEER-ARCHIVO
      *         END-PERFORM
                DISPLAY tr-det-socio
           END-PERFORM.

           PERFORM 700-FIN-GENERAL.

            STOP RUN.

       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVOS.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT TRANSACCIONES.
           OPEN OUTPUT TRANSAC-ACTUAL.

       200-LEER-ARCHIVO.
           READ TRANSACCIONES AT END MOVE 1 TO w-flag-transc.

       300-PEDIR-FECHA.
           DISPLAY "INGRESE FECHA DE TRANSACCION".
           ACCEPT w-fecha-ing.

       400-INICIO-FECHA.
           IF tr-cab-tipo IS EQUAL "C"
               AND tr-cab-fecha IS NOT EQUAL w-fecha-ing THEN
               PERFORM 430-DESAGOTAR-DETALLE

           IF w-flag-transc IS NOT EQUAL 1 THEN
               DISPLAY "NO SE REALIZARON TRANSACCIONES EN ESTA FECHA".

       430-DESAGOTAR-DETALLE.
           PERFORM UNTIL w-flag-transc IS EQUAL 1
                   OR tr-det-tipo IS EQUAL "D"
                   PERFORM 200-LEER-ARCHIVO
           END-PERFORM.

       500-INICIO-SOCIO.
       600-PROCESO-SOCIO.
       700-FIN-GENERAL.
           PERFORM 720-CERRAR-ARCHIVOS.

       720-CERRAR-ARCHIVOS.
           CLOSE TRANSACCIONES.
           CLOSE TRANSAC-ACTUAL.


       END PROGRAM YOUR-PROGRAM-NAME.
