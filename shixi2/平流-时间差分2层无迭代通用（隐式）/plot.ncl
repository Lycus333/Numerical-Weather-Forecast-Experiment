;************************************************
; NCL Graphics: 
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 
load "$NCARG_LIB/ncarg/nclscripts/csm/contributed.ncl"

;************************************************
begin

;************************************************
; set parameter
;************************************************

  Nx=500
  Nt=6
  ;0-Initial Value,1-Analytic Solutions,2-5 Numerical Solution    
  
  labels=(/"Initial Value","Analytic Solutions","Numerical Solutions"/)
  

;************************************************
; read in variables
;************************************************

  fihead = readAsciiHead("result.txt",1)
  title=fihead
  ;print(fihead)
  ;fidata=asciiread("result.txt",(/Nx,Nt+1/),"float")
  fidata=readAsciiTable("result.txt",Nt+1,"float",1)
  ;printVarSummary(fidata)
  XX = new(    Nx  , float)
  do i=0,Nx-1    
     XX(i)=fidata(i,0)
  end do
  ;print(XX)
  YY = new((/Nt,Nx/), float)
  do i=0,Nx-1
     do j=0,Nt-1
        YY(j,i)=fidata(i,j+1)
     end do
  end do
  YY3 = new((/3,Nx/), float)
  YY3(0,:)=YY(0,:)
  YY3(1,:)=YY(1,:)
  YY3(2,:)=YY(Nt-1,:)
  ;printVarSummary(YY)


;************************************************
; set default res
; rea2 for gsn_csm_xy
;************************************************
  wks   = gsn_open_wks ("ps", "Plot" )    ; open workstation
  gsn_define_colormap(wks,"gui_default")          ; choose colormap
  res2                      = True                ; plot mods desired
  res2@gsnDraw              = False          ; do not draw 
  res2@gsnFrame             = False          ; do not advance
  res2@vpXF                  = 0.15         ; x location left
  res2@vpYF                  = 0.70         ; y location  up
  res2@vpWidthF              = 0.70         ; width
  res2@vpHeightF             = 0.25         ; height
  res2@tiMainString          = title         ; title  
  res2@tiXAxisString = "space x" 
  res2@tiYAxisString = "variable u"
  ;res2@xyXStyle      = "Log"               ;Log, Linear, Irregular 
  ;res2@xyYStyle      = "Log"               ;Log, Linear, Irregular
  res2@xyXStyle      = "Linear"             ;Log, Linear, Irregular 
  res2@xyYStyle      = "Linear"             ;Log, Linear, Irregular
  ;res2@xyComputeXMin = True 
  ;res2@xyComputeXMax = True
  ;res2@xyComputeYMin = True
  ;res2@xyComputeYMax = True 
  res2@trYMinF           = -1.0               ; Y axis min 
  res2@trYMaxF           = 3.0                ; Y axis max
  res2@trXMinF           = 0                  ; X axis min 
  res2@trXMaxF           = 1000               ; X axis max
  res2@xyLineColors      = (/"RoyalBlue","red","green4","RoyalBlue","red","green4"/)  ; line colors  
  res2@xyLineThicknesses = (/1.0 ,1.0,1.0,1.0,1.0,1.0/)        ; line thicknesses
  ;res2@xyDashPatterns    = (/0  ,0  ,0  ,2  ,14 ,7 /)         ; line patterns
  res2@xyDashPatterns    = (/0  ,14  ,14 ,14 ,14 ,0  /)        ; line patterns
  res2@gsnStringFontHeightF  = 0.02
  res2@tmXBLabelFontAspectF  = 1.3
  res2@tmXBLabelFontHeightF  = 0.02       
  res2@tmYLLabelFontAspectF  = 1.3
  res2@tmYLLabelFontHeightF  = 0.02
  res2@txFontThicknessF      = 1.3      ; thickness of the lines used to draw the characters
  res2@tiXAxisFontThicknessF = 1.3
  res2@tiYAxisFontThicknessF = 1.3
  res2@tiXAxisFontHeightF    = 0.022
  res2@tiYAxisFontHeightF    = 0.022
  res2@tiMainFontHeightF     = 0.022

  res_text                    = True                  ; text mods desired
  res_text@txFontHeightF      = 0.02                  ; change text size
  res_text@txFontThicknessF   = 1.3                   ; thickness of the lines
  res_text@txJust             = "CenterLeft"          ; text justification
  res_lines                   = True                  ; polyline mods desired
  res_lines@gsLineDashPattern = 0.                    ; solid line
  res_lines@gsLineThicknessF  = 2.                    ; line thicker


;************************************************
; create plot
;************************************************   
  
  
  

  plot = gsn_csm_xy (wks,XX,YY,res2)
  draw(plot)
  frame(wks)
  
  delete(res2@xyLineColors)
  delete(res2@xyLineThicknesses)
  delete(res2@xyDashPatterns)
  res2@xyLineColors      = (/"black","black","red"/)  ; line colors  
  res2@xyLineThicknesses = (/2.5    ,2.5    ,3.0/)    ; line thicknesses
  res2@xyDashPatterns    = (/2      ,0      ,0 /)     ; line patterns
  
  plot = gsn_csm_xy (wks,XX,YY3,res2)

  pyy = 2.7
  pxx = 100
  do i=0,2
     res_text@txFontColor  = res2@xyLineColors(i)
     test_add = gsn_add_text(wks,plot,labels(i),pxx,pyy,res_text)  ; add text
     pyy = pyy-0.4   
  end do
  
  lxx = (/10 ,90 /)
  lyy = (/2.7,2.7/)
  res_lines@gsLineColor       = res2@xyLineColors(0)  
  res_lines@gsLineDashPattern = res2@xyDashPatterns(0)                 
  dum0 = gsn_add_polyline(wks,plot,lxx,lyy,res_lines)         ; add polyline
  lxx = (/10 ,90 /)
  lyy = (/2.3,2.3/)
  res_lines@gsLineColor       = res2@xyLineColors(1)  
  res_lines@gsLineDashPattern = res2@xyDashPatterns(1)          
  dum1 = gsn_add_polyline(wks,plot,lxx,lyy,res_lines)         ; add polyline
  lxx = (/10 ,90 /)
  lyy = (/1.9,1.9/)
  res_lines@gsLineColor       = res2@xyLineColors(2)  
  res_lines@gsLineDashPattern = res2@xyDashPatterns(2)           
  dum2 = gsn_add_polyline(wks,plot,lxx,lyy,res_lines)         ; add polyline
  
  draw(plot)
  frame(wks)

end


;************************************************

;"black","White","RoyalBlue","LightSkyBlue",
;"lightseagreen","PaleGreen","Wheat","Brown","Pink"

;The line style is changed using an integer from 0 to 16.
;The default is line index 0 (solid). 


