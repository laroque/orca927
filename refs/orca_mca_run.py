#!/usr/bin/python
'''
    A script file which will connect to an already configured ORCA session (ie MCA is running and the FPGA is loaded).
'''

import telnetlib
import sys

def read_next(orca_connection, maxloops=10000):
    result = None
    loops = 0
    newline = 'OrcaHeartBeat\n'
    while (newline == 'OrcaHeartBeat\n' and loops < maxloops):
        newline = orca_connection.read_until('\n', 3)
        loops = loops + 1
    return newline

def take_a_run(filename):
    print('filename is: ' + filename)
    #connect and look for a heartbeat
    orca = telnetlib.Telnet('127.0.0.1', 4667)
    if not orca.read_until('OrcaHeartBeat\n',5) == 'OrcaHeartBeat\n':
        print('did not see a heartbeat')
        sys.exit()

    #now set the various hardware params:
    #### turn of ULD
    orca.write('uld = [ORMCA927Model setUpperDiscriminator:0 withValue:' + str(2**14-1) + '];')
    #### set livetime for the run to 3000 "units" which I think is 60 seconds...
    orca.write('livelimit = [ORMCA927Model setLtPreset:0 withValue:' + str(3000) + '];')
    #### enable run ends by livetime reached, I didn't find a key but 8 seems right
    orca.write('enablelive = [ORMCA927Model setPresetCtrlReg:0 withValue:' + str(8) + '];')

    #take the run
    orca.write('start = [ORMCA927Model startAcquisition:0];')

    #save the run
    orca.write('save = [ORMCA927Model writeSpectrum:0 toFile:' + filename + '];')

# If called from the commandline, execute here
if __name__ == '__main__':
    try:
        take_a_run(*sys.argv[1:])
    except IndexError:
        print('wrong number of arguments given')
