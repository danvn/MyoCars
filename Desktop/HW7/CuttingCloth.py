# -*- coding: utf-8 -*-
"""
Created on Sun Oct 25 15:23:44 2015

@author: dannguyen
"""


import sys
def main():
    
    filename = open(sys.argv[1],"r") # Open file
    first_line = filename.readline() # Read firt line 
  
    XY = first_line.split( )         # Split with space so we can get each element
    X = int(XY[0])
    Y = int(XY[1])
    print "\nCloth dimmensions: "
    print "X =", X, "units wide"
    print "Y =", Y, "units long \n"
    
    # How many products from the cloth can we make
    n = int(float(filename.readline()))
    print "The cloth can be cut into n products:"
    print "n =", n
    
    cuts = filename.readlines()
    for i in range(0,len(cuts)):
        cuts[i] = cuts[i].strip("\n ")
        cuts[i] = cuts[i].split( )
        
    print cuts
    print cuts[1]
    
    # set up the dimmensions for the list of cuts and each of their profits
    a = []
    b = []
    c = [] 
    
    print len(a), len(b), len(c)
    #print a[0]
    
    # Make a 2 dimmensional array for both products and profits
    # Each array is X by Y
    rememberedCombos = [[0 for x in range(Y)] for x in range(X)]
    productCombos  = [[0 for x in range(Y)] for x in range(X)]
    
    for i in range(0,n-1):
    
    
    
    
    
    
if __name__ == "__main__":
    main()