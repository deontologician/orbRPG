
import pygraphviz as pgv

Combinations = [None
,('White', 'White', 'P Red')
,('White', 'Black', 'P Red')
,('White', 'P Red', 'L Lambda')
,('White', 'P Green', 'L Mu')
,('White', 'P Blue', 'L Lambda')
,('White', 'L Lambda', 'G Nyx')
,('White', 'L Mu', 'E Cesium')
,('White', 'L Omega', 'P Blue')
,('White', 'E Deuterium', 'L Lambda')
,('White', 'E Erbium', 'P Red')
,('White', 'E Cesium', 'L Mu')
,('White', 'G Nyx', 'E Deuterium')
,('White', 'G Hypnos', 'P Red')
,('White', 'G Thanatos', 'P Green')
,('White', 'D Mammon', 'L Omega')
,('White', 'D Asmodeus', 'E Deuterium')
,('White', 'D Belial', 'L Omega')
,('White', 'T Piston', 'G Nyx')
,('White', 'T Transistor', 'L Omega')
,('White', 'T Gear', 'E Deuterium')
,('Black', 'White', 'E Deuterium')
,('Black', 'Black', 'L Lambda')
,('Black', 'P Red', 'L Mu')
,('Black', 'P Green', 'P Red')
,('Black', 'P Blue', 'E Erbium')
,('Black', 'L Lambda', 'G Nyx')
,('Black', 'L Mu', 'G Nyx')
,('Black', 'L Omega', 'White')
,('Black', 'E Deuterium', 'P Red')
,('Black', 'E Erbium', 'P Red')
,('Black', 'E Cesium', 'White')
,('Black', 'G Nyx', 'E Erbium')
,('Black', 'G Hypnos', 'E Erbium')
,('Black', 'G Thanatos', 'L Lambda')
,('Black', 'D Mammon', 'P Red')
,('Black', 'D Asmodeus', 'P Blue')
,('Black', 'D Belial', 'P Red')
,('Black', 'T Piston', 'D Asmodeus')
,('Black', 'T Transistor', 'E Cesium')
,('Black', 'T Gear', 'L Mu')
,('P Red', 'White', 'L Omega')
,('P Green', 'White', 'E Deuterium')
,('P Blue', 'White', 'E Deuterium')
,('P Red', 'Black', 'L Mu')
,('P Green', 'Black', 'E Deuterium')
,('P Blue', 'Black', 'E Cesium')
,('P Red', 'P Red', 'E Deuterium')
,('P Red', 'P Green', 'L Lambda')
,('P Red', 'P Blue', 'E Deuterium')
,('P Green', 'P Red', 'L Lambda')
,('P Green', 'P Green', 'E Erbium')
,('P Green', 'P Blue', 'P Red')
,('P Blue', 'P Red', 'P Red')
,('P Blue', 'P Green', 'White')
,('P Blue', 'P Blue', 'P Blue')
,('P Red', 'L Lambda', 'G Nyx')
,('P Red', 'L Mu', 'E Deuterium')
,('P Red', 'L Omega', 'P Blue')
,('P Green', 'L Lambda', 'E Deuterium')
,('P Green', 'L Mu', 'L Lambda')
,('P Green', 'L Omega', 'L Lambda')
,('P Blue', 'L Lambda', 'E Deuterium')
,('P Blue', 'L Mu', 'E Deuterium')
,('P Blue', 'L Omega', 'P Green')
,('P Red', 'E Deuterium', 'L Lambda')
,('P Red', 'E Erbium', 'P Red')
,('P Red', 'E Cesium', 'L Lambda')
,('P Green', 'E Deuterium', 'L Lambda')
,('P Green', 'E Erbium', 'P Green')
,('P Green', 'E Cesium', 'L Omega')
,('P Blue', 'E Deuterium', 'P Red')
,('P Blue', 'E Erbium', 'E Erbium')
,('P Blue', 'E Cesium', 'D Mammon')
,('P Red', 'G Nyx', 'White')
,('P Red', 'G Hypnos', 'P Green')
,('P Red', 'G Thanatos', 'G Thanatos')
,('P Green', 'G Nyx', 'L Omega')
,('P Green', 'G Hypnos', 'D Mammon')
,('P Green', 'G Thanatos', 'D Asmodeus')
,('P Blue', 'G Nyx', 'E Deuterium')
,('P Blue', 'G Hypnos', 'E Deuterium')
,('P Blue', 'G Thanatos', 'L Mu')
,('P Red', 'D Mammon', 'T Piston')
,('P Red', 'D Asmodeus', 'P Blue')
,('P Red', 'D Belial', 'L Lambda')
,('P Green', 'D Mammon', 'E Deuterium')
,('P Green', 'D Asmodeus', 'G Nyx')
,('P Green', 'D Belial', 'P Red')
,('P Blue', 'D Mammon', 'L Mu')
,('P Blue', 'D Asmodeus', 'G Hypnos')
,('P Blue', 'D Belial', 'E Deuterium')
,('P Red', 'T Piston', 'D Asmodeus')
,('P Red', 'T Transistor', 'E Deuterium')
,('P Red', 'T Gear', 'G Hypnos')
,('P Green', 'T Piston', 'L Lambda')
,('P Green', 'T Transistor', 'P Green')
,('P Green', 'T Gear', 'D Belial')
,('P Blue', 'T Piston', 'T Gear')
,('P Blue', 'T Transistor', 'P Red')
,('P Blue', 'T Gear', 'T Transistor')
,('L Lambda', 'White', 'L Mu')
,('L Mu', 'White', 'P Blue')
,('L Omega', 'White', 'G Nyx')
,('L Lambda', 'Black', 'P Red')
,('L Mu', 'Black', 'White')
,('L Omega', 'Black', 'P Red')
,('L Lambda', 'P Red', 'E Deuterium')
,('L Lambda', 'P Green', 'G Nyx')
,('L Lambda', 'P Blue', 'L Lambda')
,('L Mu', 'P Red', 'P Red')
,('L Mu', 'P Green', 'G Nyx')
,('L Mu', 'P Blue', 'P Red')
,('L Omega', 'P Red', 'P Green')
,('L Omega', 'P Green', 'L Lambda')
,('L Omega', 'P Blue', 'E Erbium')
,('L Lambda', 'L Lambda', 'L Lambda')
,('L Lambda', 'L Mu', 'E Cesium')
,('L Lambda', 'L Omega', 'L Omega')
,('L Mu', 'L Lambda', 'G Hypnos')
,('L Mu', 'L Mu', 'L Lambda')
,('L Mu', 'L Omega', 'L Mu')
,('L Omega', 'L Lambda', 'G Nyx')
,('L Omega', 'L Mu', 'P Green')
,('L Omega', 'L Omega', 'P Red')
,('L Lambda', 'E Deuterium', 'P Red')
,('L Lambda', 'E Erbium', 'E Deuterium')
,('L Lambda', 'E Cesium', 'L Lambda')
,('L Mu', 'E Deuterium', 'G Hypnos')
,('L Mu', 'E Erbium', 'L Omega')
,('L Mu', 'E Cesium', 'G Nyx')
,('L Omega', 'E Deuterium', 'P Green')
,('L Omega', 'E Erbium', 'Black')
,('L Omega', 'E Cesium', 'D Mammon')
,('L Lambda', 'G Nyx', 'L Lambda')
,('L Lambda', 'G Hypnos', 'D Asmodeus')
,('L Lambda', 'G Thanatos', 'E Deuterium')
,('L Mu', 'G Nyx', 'E Erbium')
,('L Mu', 'G Hypnos', 'E Deuterium')
,('L Mu', 'G Thanatos', 'E Deuterium')
,('L Omega', 'G Nyx', 'D Belial')
,('L Omega', 'G Hypnos', 'White')
,('L Omega', 'G Thanatos', 'D Mammon')
,('L Lambda', 'D Mammon', 'D Asmodeus')
,('L Lambda', 'D Asmodeus', 'L Lambda')
,('L Lambda', 'D Belial', 'L Lambda')
,('L Mu', 'D Mammon', 'P Green')
,('L Mu', 'D Asmodeus', 'P Red')
,('L Mu', 'D Belial', 'D Mammon')
,('L Omega', 'D Mammon', 'T Piston')
,('L Omega', 'D Asmodeus', 'L Mu')
,('L Omega', 'D Belial', 'T Transistor')
,('L Lambda', 'T Piston', 'P Red')
,('L Lambda', 'T Transistor', 'L Lambda')
,('L Lambda', 'T Gear', 'E Deuterium')
,('L Mu', 'T Piston', 'P Blue')
,('L Mu', 'T Transistor', 'P Red')
,('L Mu', 'T Gear', 'D Asmodeus')
,('L Omega', 'T Piston', 'White')
,('L Omega', 'T Transistor', 'L Lambda')
,('L Omega', 'T Gear', 'White')
,('E Deuterium', 'White', 'L Mu')
,('E Erbium', 'White', 'Black')
,('E Cesium', 'White', 'E Erbium')
,('E Deuterium', 'Black', 'L Lambda')
,('E Erbium', 'Black', 'L Mu')
,('E Cesium', 'Black', 'D Mammon')
,('E Deuterium', 'P Red', 'G Nyx')
,('E Deuterium', 'P Green', 'L Mu')
,('E Deuterium', 'P Blue', 'G Nyx')
,('E Erbium', 'P Red', 'L Mu')
,('E Erbium', 'P Green', 'White')
,('E Erbium', 'P Blue', 'E Deuterium')
,('E Cesium', 'P Red', 'Black')
,('E Cesium', 'P Green', 'L Lambda')
,('E Cesium', 'P Blue', 'P Red')
,('E Deuterium', 'L Lambda', 'P Blue')
,('E Deuterium', 'L Mu', 'L Lambda')
,('E Deuterium', 'L Omega', 'E Erbium')
,('E Erbium', 'L Lambda', 'E Cesium')
,('E Erbium', 'L Mu', 'L Lambda')
,('E Erbium', 'L Omega', 'L Mu')
,('E Cesium', 'L Lambda', 'P Green')
,('E Cesium', 'L Mu', 'L Mu')
,('E Cesium', 'L Omega', 'L Lambda')
,('E Deuterium', 'E Deuterium', 'E Erbium')
,('E Deuterium', 'E Erbium', 'Black')
,('E Deuterium', 'E Cesium', 'G Nyx')
,('E Erbium', 'E Deuterium', 'L Mu')
,('E Erbium', 'E Erbium', 'L Mu')
,('E Erbium', 'E Cesium', 'P Red')
,('E Cesium', 'E Deuterium', 'L Lambda')
,('E Cesium', 'E Erbium', 'P Red')
,('E Cesium', 'E Cesium', 'P Red')
,('E Deuterium', 'G Nyx', 'E Cesium')
,('E Deuterium', 'G Hypnos', 'P Green')
,('E Deuterium', 'G Thanatos', 'P Green')
,('E Erbium', 'G Nyx', 'L Mu')
,('E Erbium', 'G Hypnos', 'G Thanatos')
,('E Erbium', 'G Thanatos', 'P Red')
,('E Cesium', 'G Nyx', 'D Asmodeus')
,('E Cesium', 'G Hypnos', 'G Nyx')
,('E Cesium', 'G Thanatos', 'P Green')
,('E Deuterium', 'D Mammon', 'L Lambda')
,('E Deuterium', 'D Asmodeus', 'E Deuterium')
,('E Deuterium', 'D Belial', 'D Asmodeus')
,('E Erbium', 'D Mammon', 'Black')
,('E Erbium', 'D Asmodeus', 'E Erbium')
,('E Erbium', 'D Belial', 'D Mammon')
,('E Cesium', 'D Mammon', 'E Erbium')
,('E Cesium', 'D Asmodeus', 'G Thanatos')
,('E Cesium', 'D Belial', 'E Deuterium')
,('E Deuterium', 'T Piston', 'Black')
,('E Deuterium', 'T Transistor', 'P Red')
,('E Deuterium', 'T Gear', 'G Nyx')
,('E Erbium', 'T Piston', 'P Red')
,('E Erbium', 'T Transistor', 'P Red')
,('E Erbium', 'T Gear', 'E Deuterium')
,('E Cesium', 'T Piston', 'P Red')
,('E Cesium', 'T Transistor', 'Black')
,('E Cesium', 'T Gear', 'E Deuterium')
,('G Nyx', 'White', 'G Nyx')
,('G Hypnos', 'White', 'P Red')
,('G Thanatos', 'White', 'L Lambda')
,('G Nyx', 'Black', 'G Nyx')
,('G Hypnos', 'Black', 'E Deuterium')
,('G Thanatos', 'Black', 'P Red')
,('G Nyx', 'P Red', 'D Mammon')
,('G Nyx', 'P Green', 'Black')
,('G Nyx', 'P Blue', 'L Lambda')
,('G Hypnos', 'P Red', 'E Cesium')
,('G Hypnos', 'P Green', 'G Nyx')
,('G Hypnos', 'P Blue', 'D Asmodeus')
,('G Thanatos', 'P Red', 'L Lambda')
,('G Thanatos', 'P Green', 'L Lambda')
,('G Thanatos', 'P Blue', 'P Red')
,('G Nyx', 'L Lambda', 'T Transistor')
,('G Nyx', 'L Mu', 'D Mammon')
,('G Nyx', 'L Omega', 'White')
,('G Hypnos', 'L Lambda', 'White')
,('G Hypnos', 'L Mu', 'G Nyx')
,('G Hypnos', 'L Omega', 'G Nyx')
,('G Thanatos', 'L Lambda', 'D Belial')
,('G Thanatos', 'L Mu', 'D Asmodeus')
,('G Thanatos', 'L Omega', 'T Piston')
,('G Nyx', 'E Deuterium', 'E Deuterium')
,('G Nyx', 'E Erbium', 'D Mammon')
,('G Nyx', 'E Cesium', 'P Red')
,('G Hypnos', 'E Deuterium', 'E Deuterium')
,('G Hypnos', 'E Erbium', 'P Blue')
,('G Hypnos', 'E Cesium', 'P Red')
,('G Thanatos', 'E Deuterium', 'E Deuterium')
,('G Thanatos', 'E Erbium', 'L Mu')
,('G Thanatos', 'E Cesium', 'P Red')
,('G Nyx', 'G Nyx', 'P Green')
,('G Nyx', 'G Hypnos', 'E Deuterium')
,('G Nyx', 'G Thanatos', 'L Lambda')
,('G Hypnos', 'G Nyx', 'L Omega')
,('G Hypnos', 'G Hypnos', 'E Deuterium')
,('G Hypnos', 'G Thanatos', 'L Lambda')
,('G Thanatos', 'G Nyx', 'L Lambda')
,('G Thanatos', 'G Hypnos', 'D Mammon')
,('G Thanatos', 'G Thanatos', 'L Mu')
,('G Nyx', 'D Mammon', 'L Omega')
,('G Nyx', 'D Asmodeus', 'P Red')
,('G Nyx', 'D Belial', 'Black')
,('G Hypnos', 'D Mammon', 'P Red')
,('G Hypnos', 'D Asmodeus', 'T Transistor')
,('G Hypnos', 'D Belial', 'G Nyx')
,('G Thanatos', 'D Mammon', 'L Mu')
,('G Thanatos', 'D Asmodeus', 'E Erbium')
,('G Thanatos', 'D Belial', 'G Hypnos')
,('G Nyx', 'T Piston', 'L Lambda')
,('G Nyx', 'T Transistor', 'P Green')
,('G Nyx', 'T Gear', 'T Transistor')
,('G Hypnos', 'T Piston', 'P Red')
,('G Hypnos', 'T Transistor', 'P Blue')
,('G Hypnos', 'T Gear', 'P Blue')
,('G Thanatos', 'T Piston', 'P Green')
,('G Thanatos', 'T Transistor', 'G Nyx')
,('G Thanatos', 'T Gear', 'E Erbium')
,('D Mammon', 'White', 'G Hypnos')
,('D Asmodeus', 'White', 'L Omega')
,('D Belial', 'White', 'P Red')
,('D Mammon', 'Black', 'T Piston')
,('D Asmodeus', 'Black', 'E Erbium')
,('D Belial', 'Black', 'G Hypnos')
,('D Mammon', 'P Red', 'L Omega')
,('D Mammon', 'P Green', 'P Red')
,('D Mammon', 'P Blue', 'P Blue')
,('D Asmodeus', 'P Red', 'E Cesium')
,('D Asmodeus', 'P Green', 'L Mu')
,('D Asmodeus', 'P Blue', 'E Erbium')
,('D Belial', 'P Red', 'G Nyx')
,('D Belial', 'P Green', 'P Red')
,('D Belial', 'P Blue', 'Black')
,('D Mammon', 'L Lambda', 'P Red')
,('D Mammon', 'L Mu', 'G Hypnos')
,('D Mammon', 'L Omega', 'L Omega')
,('D Asmodeus', 'L Lambda', 'E Erbium')
,('D Asmodeus', 'L Mu', 'G Nyx')
,('D Asmodeus', 'L Omega', 'G Nyx')
,('D Belial', 'L Lambda', 'L Mu')
,('D Belial', 'L Mu', 'D Asmodeus')
,('D Belial', 'L Omega', 'P Red')
,('D Mammon', 'E Deuterium', 'L Mu')
,('D Mammon', 'E Erbium', 'E Erbium')
,('D Mammon', 'E Cesium', 'E Cesium')
,('D Asmodeus', 'E Deuterium', 'P Green')
,('D Asmodeus', 'E Erbium', 'P Red')
,('D Asmodeus', 'E Cesium', 'G Nyx')
,('D Belial', 'E Deuterium', 'G Nyx')
,('D Belial', 'E Erbium', 'G Hypnos')
,('D Belial', 'E Cesium', 'P Green')
,('D Mammon', 'G Nyx', 'Black')
,('D Mammon', 'G Hypnos', 'D Asmodeus')
,('D Mammon', 'G Thanatos', 'P Green')
,('D Asmodeus', 'G Nyx', 'D Belial')
,('D Asmodeus', 'G Hypnos', 'D Asmodeus')
,('D Asmodeus', 'G Thanatos', 'Black')
,('D Belial', 'G Nyx', 'G Nyx')
,('D Belial', 'G Hypnos', 'White')
,('D Belial', 'G Thanatos', 'Black')
,('D Mammon', 'D Mammon', 'G Thanatos')
,('D Mammon', 'D Asmodeus', 'P Green')
,('D Mammon', 'D Belial', 'L Mu')
,('D Asmodeus', 'D Mammon', 'T Transistor')
,('D Asmodeus', 'D Asmodeus', 'E Erbium')
,('D Asmodeus', 'D Belial', 'P Red')
,('D Belial', 'D Mammon', 'P Green')
,('D Belial', 'D Asmodeus', 'G Thanatos')
,('D Belial', 'D Belial', 'P Red')
,('D Mammon', 'T Piston', 'L Lambda')
,('D Mammon', 'T Transistor', 'E Cesium')
,('D Mammon', 'T Gear', 'L Omega')
,('D Asmodeus', 'T Piston', 'L Lambda')
,('D Asmodeus', 'T Transistor', 'E Erbium')
,('D Asmodeus', 'T Gear', 'D Mammon')
,('D Belial', 'T Piston', 'L Mu')
,('D Belial', 'T Transistor', 'L Lambda')
,('D Belial', 'T Gear', 'L Omega')
,('T Piston', 'White', 'L Mu')
,('T Transistor', 'White', 'G Hypnos')
,('T Gear', 'White', 'P Red')
,('T Piston', 'Black', 'L Mu')
,('T Transistor', 'Black', 'G Nyx')
,('T Gear', 'Black', 'E Cesium')
,('T Piston', 'P Red', 'L Mu')
,('T Piston', 'P Green', 'P Green')
,('T Piston', 'P Blue', 'G Nyx')
,('T Transistor', 'P Red', 'L Lambda')
,('T Transistor', 'P Green', 'D Asmodeus')
,('T Transistor', 'P Blue', 'L Mu')
,('T Gear', 'P Red', 'L Omega')
,('T Gear', 'P Green', 'T Piston')
,('T Gear', 'P Blue', 'P Red')
,('T Piston', 'L Lambda', 'G Nyx')
,('T Piston', 'L Mu', 'E Erbium')
,('T Piston', 'L Omega', 'L Lambda')
,('T Transistor', 'L Lambda', 'L Mu')
,('T Transistor', 'L Mu', 'G Nyx')
,('T Transistor', 'L Omega', 'P Blue')
,('T Gear', 'L Lambda', 'L Lambda')
,('T Gear', 'L Mu', 'G Nyx')
,('T Gear', 'L Omega', 'D Mammon')
,('T Piston', 'E Deuterium', 'L Omega')
,('T Piston', 'E Erbium', 'L Omega')
,('T Piston', 'E Cesium', 'L Mu')
,('T Transistor', 'E Deuterium', 'E Cesium')
,('T Transistor', 'E Erbium', 'G Nyx')
,('T Transistor', 'E Cesium', 'D Asmodeus')
,('T Gear', 'E Deuterium', 'L Lambda')
,('T Gear', 'E Erbium', 'E Erbium')
,('T Gear', 'E Cesium', 'P Blue')
,('T Piston', 'G Nyx', 'T Gear')
,('T Piston', 'G Hypnos', 'L Lambda')
,('T Piston', 'G Thanatos', 'L Mu')
,('T Transistor', 'G Nyx', 'G Hypnos')
,('T Transistor', 'G Hypnos', 'L Lambda')
,('T Transistor', 'G Thanatos', 'P Red')
,('T Gear', 'G Nyx', 'T Piston')
,('T Gear', 'G Hypnos', 'D Mammon')
,('T Gear', 'G Thanatos', 'T Piston')
,('T Piston', 'D Mammon', 'L Lambda')
,('T Piston', 'D Asmodeus', 'G Nyx')
,('T Piston', 'D Belial', 'E Deuterium')
,('T Transistor', 'D Mammon', 'D Asmodeus')
,('T Transistor', 'D Asmodeus', 'P Red')
,('T Transistor', 'D Belial', 'L Mu')
,('T Gear', 'D Mammon', 'T Transistor')
,('T Gear', 'D Asmodeus', 'D Asmodeus')
,('T Gear', 'D Belial', 'L Mu')
,('T Piston', 'T Piston', 'E Cesium')
,('T Piston', 'T Transistor', 'G Nyx')
,('T Piston', 'T Gear', 'E Deuterium')
,('T Transistor', 'T Piston', 'E Deuterium')
,('T Transistor', 'T Transistor', 'E Deuterium')
,('T Transistor', 'T Gear', 'White')
,('T Gear', 'T Piston', 'G Nyx')
,('T Gear', 'T Transistor', 'L Lambda')
,('T Gear', 'T Gear', 'G Nyx')

]
Combinations.remove(None)

G = pgv.AGraph(strict = True, directed = True,
               fontsize = 12, size = "7.75,10.5")

def addedge((a,b,c)):
    G.add_edge(a,c,b)
    G.add_edge(b,c,a)

for C in Combinations:
    addedge(C)

# twopi, gvcolor, wc, ccomps, tred, sccmap, fdp, 
# circo, neato, acyclic, nop, gvpr, dot.

G.layout('circo')
G.draw("combinations.pdf")