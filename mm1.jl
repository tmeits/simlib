# Main program for single-server queueing system.
const qlimit = 100
type model
    aniq:: Float64
    autil:: Float64
    busy:: Int
    idle:: Int
    marrvt:: Float64
    mservt:: Float64
    nevnts:: Int
    next:: Int
    niq:: Int
    numcus:: Int
    server:: Int
    tarrvl:: Array{Float64,1}
    time:: Float64
    tlevnt:: Float64
    tne:: Array{Float64,1}
    totcus:: Int
    totdel:: Float64 
end


