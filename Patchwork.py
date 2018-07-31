from graphics import *
def drawPatch2(originX, originY, colour, win):
    for i in range(0, 100, 20):
        bottomleftvertical = Point(originX + i, originY+100)
        topRightVert = Point(originX + i + 10, originY - i + 90)
        rectangleVert = Rectangle(bottomleftvertical, topRightVert)
        bottomleftHorz = Point(originX, originY - i +100)
        topRightHorz = Point(originX + i+ 10, originY - i +90)
        rectangleHorz = Rectangle(bottomleftHorz, topRightHorz)
        rectangleVert.setFill(colour)
        rectangleHorz.setFill(colour)
        rectangleHorz.setOutline("")
        rectangleVert.setOutline("")
        rectangleHorz.draw(win)
        rectangleVert.draw(win)
          
def drawPatch1(originX, originY, colour, win):
    boundary = originY+33
    for xCoord in range(originX, originX + 100, 25):
        for yCoord in range(originY, originY + 99, 33):
            sail1 = Point(xCoord+12.5, yCoord)
            sail2 = Point(xCoord, yCoord+15)
            sail3 = Point(xCoord+25, yCoord+15)
            sail = Polygon(sail1, sail2, sail3)
            mast = Line(Point(xCoord+12.5, yCoord+15), Point(xCoord+12.5, yCoord+23))
            hull1 = Point(xCoord, yCoord+23)
            hull2 = Point(xCoord+25, yCoord+23)
            hull3 = Point(xCoord+2, yCoord+30)
            hull4 = Point(xCoord+23, yCoord+30)
            hull = Polygon(hull1, hull2, hull4, hull3)
            if yCoord!=boundary:
                sail.setFill(colour)
                sail.setOutline("")
            else:
                sail.setFill("")
            sail.draw(win)
            mast.draw(win)
            hull.draw(win)
            
def drawPatch(Patchnum, colour, xCoord, yCoord, win):
    if Patchnum ==1:
        drawPatch1(xCoord, yCoord, colour, win) 
    else:
        drawPatch2(xCoord, yCoord, colour, win) 
    
def changeColours(win, size, colourlist):
    patch = []
    for i in range(size):
        patchline = []
        for j in range(size):
            if (j==((size-1)*100)) or (i==0) or (j==i):
                patchline.append(2)
            if i>j:
                patchline.append(1)
            elif j>i:
                patchline.append(0)
        patch.append(patchline)
    
    while True:
        p = win.getMouse()
        x = p.getX() // 100
        y = p.getY() // 100
        patch[y][x] += 1
        if patch[y][x]>2:
            patch[y][x] = 0
        Patchnum = 1
        if x == size - 1 or y == 0 or x==y:
            Patchnum = 2
            
        drawPatch(Patchnum, colourlist[patch[y][x]], x*100, y*100, win)
    
def Main():
    colournum = 0 
    colourlist = []
    validcolours = ["red", "green", "blue", "orange", "brown", "pink"]
    validsizes = [5, 7, 9]
    colour = ""
    colourcount = 0
    sizecount = 0
    while sizecount == 0 :
        size = eval(input("What is the size of the patch: "))
        if size in validsizes:
            sizecount = sizecount + 1
        else:
            print(size, "is invalid, please try again. Valid sizes are 5, 7, 9")
            
    while colourcount!=3:
        colour = input("What colour do you want: ")
        if colour in validcolours and not (colour in colourlist):
            colourcount = colourcount + 1
            colourlist.append(colour)
        else:
            print(colour, " is invalid, try again. Valid colours are:", str(validcolours).strip("[]"))
    win = GraphWin("Patchwork", size*100, size*100)
    for xCoord in range(0, size*100, 100):
        for yCoord in range(0, size*100, 100):
            Patchnum = 1
            if (xCoord==((size-1)*100)) or (yCoord==0) or (xCoord==yCoord):
                Patchnum = 2
                colournum = 0
            if yCoord>xCoord:
                colournum = 2
            elif xCoord>yCoord:
                colournum = 1
            drawPatch(Patchnum, colourlist[colournum], xCoord, yCoord, win)
    changeColours(win, size, colourlist)
    
Main()
                
    