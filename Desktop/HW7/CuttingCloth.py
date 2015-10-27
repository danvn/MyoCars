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
    
    products = filename.readlines()
    for i in range(0,len(products)):
        products[i] = products[i].strip("\n ")
        products[i] = products[i].split( )
        
    products.pop() #get rid of empty last element
    print products
        
    # Make a 2 dimmensional array for both products and profits
    # Each array is X by Y
    xCoord = [0 for x in range(X)] 
    yCoord  = [[0 for x in range(X)] for x in range(Y)]
    
    # Store the a list of the profit for each product we are given    
    profits = []
    print "prices"
    for i in range(0,n):
        c = int(products[i][2])
        print c
        profits.append(c)
    

#def cleanCut()
    
    
    
    
    
    
    
if __name__ == "__main__":
    main()