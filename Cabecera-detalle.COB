      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ******************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCH-TRANS
           ASSIGN TO "../transacciones.dat"
           ORGANIZATION LINE SEQUENTIAL.
      ******************************************************************
       DATA DIVISION.
      ******************************************************************

       FILE SECTION.
      ******************************************************************
      **************************ARCHIVOS********************************
      ******************************************************************
       FD  ARCH-TRANS.
       01  tr-cab-reg.
           03 tr-cab-tipo PIC X.
           03 tr-cab-fecha PIC 9(8).
       01 tr-det-reg.
           03 tr-det-tipo PIC X.
           03 tr-det-socio PIC 9999.
           03 tr-det-importe PIC S9(7)V99.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      ******************************************************************
      **************************PROGRAMA PRINCIPAL**********************
      ******************************************************************
           PERFORM 100-INICIO.
           PERFORM 200-INGRESO-FECHA.
           PERFORM UNTIL tr-cab-fecha=0
               PERFORM 600-GRABO-FECHA
               PERFORM 250-INICIO-SOCIO
               PERFORM 300-INGRESO-SOCIO
               PERFORM UNTIL tr-det-socio =0
                   PERFORM 400-INGRESO-RESTO
                   PERFORM 500-GRABO-SOCIO
                   PERFORM 300-INGRESO-SOCIO
               END-PERFORM
               PERFORM 200-INGRESO-FECHA
           END-PERFORM.
           PERFORM 700-FIN.
      ******************************************************************
      ************************RUTINAS***********************************
      ******************************************************************
       100-INICIO.
           OPEN OUTPUT ARCH-TRANS.

       200-INGRESO-FECHA.
           PERFORM 220-TRAIGO-FECHA.
           PERFORM UNTIL tr-cab-fecha >= 0
              display "Error, te dije un numero positivo, x fin 0"
              PERFORM 220-TRAIGO-FECHA
           END-PERFORM.

       220-TRAIGO-FECHA.
           display "Ingrese una fecha  (> 0), por fin ingrese 0".
           accept tr-cab-fecha.

       250-INICIO-SOCIO.
           MOVE "D" TO tr-det-tipo.

       300-INGRESO-SOCIO.
           PERFORM 320-TRAIGO-SOCIO.
           PERFORM UNTIL tr-det-socio >= 0
              display "Error, te dije un numero positivo, x fin 0"
              PERFORM 320-TRAIGO-SOCIO
           END-PERFORM.

       320-TRAIGO-SOCIO.
           display "Ingrese el nro. de socio (> 0), por fin ingrese 0".
           accept tr-det-socio.


       400-INGRESO-RESTO.
           display "Ingrese el importe de la transaccion".
           ACCEPT tr-det-importe.

       500-GRABO-SOCIO.
           WRITE tr-det-reg.

       600-GRABO-FECHA.
           MOVE "C" TO tr-cab-tipo.
           WRITE tr-cab-reg.

       700-FIN.
           CLOSE ARCH-TRANS.

           STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
