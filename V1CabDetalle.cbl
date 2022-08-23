      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CECILIA-OLMOS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACCIONES
           ASSIGN TO
            "..\transacciones.txt"
            ORGANIZATION is line SEQUENTIAL.
           SELECT TRANSACC-ACT
           ASSIGN TO
           "..\trans_act.dat"
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

       FD  TRANSACC-ACT.
       01  tra-reg.
           03 tra-socio pic 9(4).
           03 tra-importe pic s9(8)v99.
       WORKING-STORAGE SECTION.
       01  w-fecha-ing pic 9(8).
       01  w-flag-trans pic 9 value zero.
       01  w-soc-ant pic 9(4).
       01  w-tot-soc pic s9(7)v99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 300-LEER-ARCHIVO.
           PERFORM 200-INGRESO-FECHA.

           PERFORM UNTIL w-flag-trans =1
               PERFORM 400-DESAGOTAR-DETALLE
               PERFORM UNTIL w-flag-tranS=1 OR tr-cab-tipo="C"
                   PERFORM 300-LEER-ARCHIVO
               END-PERFORM
           END-PERFORM.
           PERFORM 800-FIN.
            STOP RUN.
       100-INICIO.
           PERFORM 120-ABRIR-ARCHIVOS.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT TRANSACCIONES.
           OPEN OUTPUT TRANSACC-ACT.
       200-INGRESO-FECHA.
           DISPLAY "INGRESE UNA FECHA, CERO PARA TERMINAR".
           ACCEPT w-fecha-ing.
           PERFORM 220-VALIDAR-FECHA.
       220-VALIDAR-FECHA.
           PERFORM UNTIL w-fecha-ing is >= ZERO
               DISPLAY "ERROR, ingrese una fecha AAAAMMDD, cero para
      -         "terminar"
                DISPLAY "INGRESE UNA FECHA, CERO PARA TERMINAR"
               ACCEPT w-fecha-ing
           END-PERFORM.
       300-LEER-ARCHIVO.
           READ TRANSACCIONES AT END MOVE 1 TO w-flag-trans.
       400-DESAGOTAR-DETALLE.
           IF tr-cab-tipo= "C"
               if tr-cab-fecha=w-fecha-ing
                   DISPLAY tr-cab-fecha
                   PERFORM 300-LEER-ARCHIVO
                   PERFORM 500-PROCESO-DETALLE
               end-if
           PERFORM 300-LEER-ARCHIVO
           END-IF.
       500-PROCESO-DETALLE.
           PERFORM UNTIL w-flag-trans=1 OR tr-cab-tipo="C"
               PERFORM 520-INICIO-SOCIO
               PERFORM UNTIL w-flag-trans=1 OR tr-cab-tipo="C"
               OR tr-det-socio IS NOT = w-soc-ant
                 ADD tr-det-importe TO w-tot-soc
                PERFORM 300-LEER-ARCHIVO
                END-PERFORM
               PERFORM 530-FIN-SOCIO
           END-PERFORM.
       520-INICIO-SOCIO.
           move tr-det-socio to w-soc-ant.
           move zero to w-tot-soc.
       530-FIN-SOCIO.
           DISPLAY w-soc-ant, "ACUMULO ", w-tot-soc.
       800-FIN.
           CLOSE TRANSACCIONES TRANSACC-ACT.
       END PROGRAM CECILIA-OLMOS.
