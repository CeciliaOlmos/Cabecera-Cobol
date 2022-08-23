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
           "..\Archivo\transacciones.txt"
           ORGANIZATION is line sequential.

           SELECT TRANSACCIONES-ACT
           ASSIGN TO
           "..\Archivo\transacciones_act.txt"
           ORGANIZATION is line sequential.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACCIONES.
       01  tr-cab-reg.
           03 tr-cab-tipo pic x.
           03 tr-cab-fecha PIC 9(8).
       01  tr-det-reg.
           03 tr-det-tipo pic x.
           03 tr-det-socio pic 9(4).
           03 tr-det-importe pic s9(7)v99.

       FD  TRANSACCIONES-ACT.
       01  tra-reg.
           03 tra-socio pic 9(4).
           03 tra-importe pic s9(8)v99.

       WORKING-STORAGE SECTION.
       01  w-flag-transc pic 9 value zero.
       01  w-fecha-ing pic 9(8).
       01  w-socio-ant pic 9(4).
       01  w-fecha-existe pic x value 'N'.
       01  w-fecha-menor pic x value 'N'.
       01  w-resto-fecha pic s9(8).
       01  w-soc-acum-imp pic s9(8)v99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 130-PIDO-FECHA-Y-BUSCO.
            PERFORM 170-MENSAJE-ERROR UNTIL w-fecha-existe IS EQUALS 'S'
                                         OR w-flag-transc IS EQUALS 1
            IF w-fecha-existe IS EQUALS 'S' THEN
                PERFORM 250-PROCESO-FECHA
            END-IF.

            PERFORM 700-FIN-GENERAL.
           STOP RUN.

       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVOS.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT TRANSACCIONES.
           OPEN OUTPUT TRANSACCIONES-ACT.

       130-PIDO-FECHA-Y-BUSCO.
           PERFORM 210-INGRESO-FECHA.
           PERFORM 220-BUSCO-FECHA.

       150-LEER-TRANSACCION.
           READ TRANSACCIONES AT END MOVE 1 TO w-flag-transc.

       170-MENSAJE-ERROR.
           DISPLAY "ERROR!! NO SE ENCUENTRA LA FECHA EN EL ARCHIVO".
           PERFORM 130-PIDO-FECHA-Y-BUSCO.

       200-INICIO-FECHA.

       210-INGRESO-FECHA.
           DISPLAY "INGRESE FECHA:"
           ACCEPT w-fecha-ing.

       220-BUSCO-FECHA.
           PERFORM 150-LEER-TRANSACCION.
           PERFORM UNTIL w-flag-transc IS EQUALS 1 OR
                         w-fecha-existe IS EQUALS 'S' OR
                         w-fecha-menor IS EQUALS 'S'
               PERFORM 230-EVALUO-FECHA
           END-PERFORM.
           IF w-fecha-menor IS EQUALS 'S'
               MOVE 'N' TO w-fecha-menor.

       230-EVALUO-FECHA.
           IF tr-cab-tipo IS EQUALS 'C' THEN
               COMPUTE w-resto-fecha = w-fecha-ing - tr-cab-fecha
               IF w-resto-fecha < ZERO THEN
                   MOVE 'S' TO w-fecha-menor
               ELSE
                   IF w-resto-fecha IS EQUALS ZERO THEN
                       MOVE 'S' TO w-fecha-existe
                   END-IF

                END-IF

           ELSE
               PERFORM 235-DESPACHO-DETALLE
           END-IF.

       235-DESPACHO-DETALLE.
           PERFORM UNTIL w-flag-transc IS EQUALS 1 OR
                         tr-cab-tipo IS EQUALS 'C'
               PERFORM 150-LEER-TRANSACCION
           END-PERFORM.

       250-PROCESO-FECHA.
           PERFORM 150-LEER-TRANSACCION.
           PERFORM UNTIL w-flag-transc IS EQUALS 1 OR
                         tr-det-tipo IS EQUALS 'C'
               PERFORM 300-INICIO-SOCIO
               PERFORM UNTIL w-flag-transc IS EQUALS 1 OR
                             tr-det-tipo IS EQUALS 'C' OR
                             tr-det-socio IS NOT EQUALS w-socio-ant
                   PERFORM 400-PROCESO-SOCIO
                   PERFORM 150-LEER-TRANSACCION
               END-PERFORM
               PERFORM 500-FIN-SOCIO
           END-PERFORM.

       300-INICIO-SOCIO.
           MOVE tr-det-socio TO w-socio-ant.
           MOVE ZERO TO w-soc-acum-imp.

       400-PROCESO-SOCIO.
           ADD tr-det-importe TO w-soc-acum-imp.

       500-FIN-SOCIO.
           PERFORM 510-ARMO-TRANS-ACT-REG.
           WRITE tra-reg.

       510-ARMO-TRANS-ACT-REG.
           MOVE w-socio-ant TO tra-socio.
           MOVE w-soc-acum-imp TO tra-importe.

       600-FIN-FECHA.

       700-FIN-GENERAL.
           PERFORM 720-CERRAR-ARCHIVOS.

       720-CERRAR-ARCHIVOS.
           CLOSE TRANSACCIONES.
           CLOSE TRANSACCIONES-ACT.

       END PROGRAM YOUR-PROGRAM-NAME.
