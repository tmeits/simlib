# Main program for single-server queueing system.
module ssqs

importall Base
const 
    qlimit = 100
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

# Constructor definition
function model(nevnts:: Int, 
               busy:: Int, 
               idle:: Int,
               marrvt:: Float64, 
               mservt:: Float64, 
               totcus:: Int)
#Initialize the simulation clock.
    time = 0.0 
# Initialize the state variables.       
    server = idle 
    niq = 0
    tlevnt = 0.0
# Initialize the statistical counters.
    numcus = 0
    totdel = 0.0
    aniq = 0.0
    autil = 0.0
# Initialize event list. Since no customers are present, the
# departure (service completion) event is eliminated from
# consideration.
    tne = [time + exp(marrvt), 1.0e+30]

    next = 0.
    tarrvl = rand(qlimit)

    new(
        aniq,
        autil,
        busy,
        idle,
        marrvt,
        mservt,
        nevnts,
        next,
        niq,
        numcus,
        server,
        tarrvl,
        time,
        tlevnt,
        tne,
        totcus,
        totdel
    )    
    
end
end
end
# Specify the number of event types for the timing routine.
 
nevnts = 2
        
# Set mnemonics for server's being busy and idle.

busy = 1
idle = 0

# Set input parameters.
		     
marrvt = 1.
mservt = .5
totcus = 1000

# Create our new queuing system
qs = model(2, 1, 0, 1., .5, 1000)

function timing(qs:: model)
end

function timing(qs:: model)
end

function timing(qs:: model)
end

function timing(qs:: model)
end

function timing(qs:: model)
end

# http://habrahabr.ru/post/28108/
# http://habrahabr.ru/post/131951/
# http://stackoverflow.com/questions/1218390/what-is-your-most-productive-shortcut-with-vim

type mmm5 
    m::Array{Float64,1} 
    function mmm5() 
        m=[1.,2.]
        new(m)
    end 
end

