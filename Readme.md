M0GBP-RTTY is an interface unit which interfaces a creed teleprinter to a number of data sources. It also provides test facilities.

In addition to an interface to the Creed printer is has:

•	An interface to a PC using a RS232 serial link. This could also be used to interface to a teletype or a VDU.

•	A modulator and demodulator for interfacing to a TRx. The system can transmit and receive ham signals at 45.45 baud or receive DDK weather station signals at 50 baud.

•	A local LCD display which shows the received signals in scrolling mode.

The system is based on a PAG TU which I was given. I disconnected its control unit and used its modulator, demodulator, motor control and Creed magnet control connected to a PIC microprocessor. The PIC:

.	Converts between baud rates

•	Converts between ASCII and ITA codes

•	Routes signals depending on mode

•	Implement Autostart, USOS, Diddles, Auto LFCR, Reverse
