
# https://www.lawa.org.nz/media/16576/nems-dissolved-oxygen-recording-2013-06-1-.pdf

temp_c = 20
temp_k = temp_c + 273.15
salinity = 0

lnCs = -139.344 + 1.575701*10^5/temp_k - 6.642308*10^7/temp_k^2 + 1.2438*10^10/temp_k^3 - 8.621949*10^11/temp_k^4 -
  salinity*(-1.7674*10^-2 - 1.0754*10^1/temp_k + 2.1407*10^3/temp_k^2)

saturation_o2mgl = exp(lnCs)
saturation_o2mgl
