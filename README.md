<!DOCTYPE html>
<html lang="en">
  <head>

    <!-- Basic Page Needs
         –––––––––––––––––––––––––––––––––––––––––––––––––– -->
    <meta charset="utf-8">
    <title>Resume</title>
    <meta name="description" content="">
    <meta name="author" content="">

    <!-- Mobile Specific Metas
         –––––––––––––––––––––––––––––––––––––––––––––––––– -->
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <!-- FONT
         –––––––––––––––––––––––––––––––––––––––––––––––––– -->
    <link href="//fonts.googleapis.com/css?family=Raleway:400,300,600" rel="stylesheet" type="text/css">

    <!-- CSS
         –––––––––––––––––––––––––––––––––––––––––––––––––– -->
    <link rel="stylesheet" href="css/normalize.css">
    <link rel="stylesheet" href="css/skeleton.css">

    <!-- Favicon%
         –––––––––––––––––––––––––––––––––––––––––––––––––– -->
    <link rel="icon" type="image/png" href="images/favicon.png">

  </head>
  <body>

    <!-- Primary Page Layout
         –––––––––––––––––––––––––––––––––––––––––––––––––– -->
    <div class="container">
      <div class="row">
        <div class="one-half column" style="margin-top: 5%">
          <h3>Experience</h3>
          <p>
            Software Development Engineer, Technodigm Innovation (Aug 2014-Present)
            <ul>
              <li>
                <em>Inline Dispensing System</em> - A high speed gantry robot for PCB dispensing which was operated by software written in Visual Basic .NET 2003. The original project was undertaken by SIMTech but scrapped halfway.
                <ul>
                  <li>Bug hunter.</li>
                  <li>Fixing the software build.</li>
                  <li>Testing for ease of use and software stability.</li>
                  <li>Troubleshooting sensor communication and readings.</li>
                  <li>Linear motor and servo drive troubleshooting and tuning.</li>
                  <li>Implementing a finite state machine to handle machine state transitions.</li>
                  <li>Refactoring the project by aggressively cleaning up and removing dead code.</li>
                  <li>Rewriting libraries with Visual Basic's optional strict typing enabled where possible, to reduce run-time bugs.</li>
                  <li>Optimizations in cycle time (time taken to dispense on one board) through various hacks i.e. calculating motion trajectory in the motion controller software instead of downloading the entire set of points every single time, without affecting run time speed.</li>
                </ul>
              </li>
              
              <li><em>DXF Converter</em> - Desktop software for converting DXF files into the company's proprietary motion path formats for the robot to parse.
                <ul>
                  <li>GUI display supports simple editing.</li>
                  <li>Simple nearest neighbor algorithm to optimize the dispensing elements.</li>
                </ul>
              </li>
              
              <li>Misc responsibilities
                <ul>
                  <li>PLC programming for customized projects.</li>
                  <li>Software prototypes for a new graphical teaching interface instead of move and teach interface for gantry robots.</li>
                  <li>Some research on what it would take to implement a soft real-time system to do "vision on the fly", which will allow the camera to capture images without the gantry stopping for positioning and inspection purposes. Software would need to trigger the camera at set encoder points, recalculate positioning from processing the image of position marker, and send the updated coordinates to the motion controller fast enough while taking into account the latency to trigger the camera.</li>
                </ul>
              </li>
            </ul>
          </p>
        </div>
      </div>
      <div class="row">
        <div class="one-half column" style="margin-top: 5%">
          <h3>Education</h3>
          <p>
            Bachelor's of Mechatronics (2012-2014)
            <br>
            Singapore Institute of Technology - University of Glasgow (Accelerated 2-year program)
            <br>
            Linear Algebra, Digital Signal Processing (and a poor understanding of Control Theory, Dynamic Programming, Finite Element Analysis)
            <br>
            <br>
            Diploma in Clean Energy Management (2009-2012)
            <br>
            Ngee Ann Polytechnic
            <br>
            Boolean Algebra, Calculus, Electronics
          </p>
        </div>
      </div>
      <div class="row">
        <div class="one-half column" style="margin-top: 5%">
          <h3>Chia Kang Ren</h3>
          <p>
            Singaporean, Chinese Male
            <br>
            Skills
            <br>
            Languages: Racket, Clojure/Clojurescript, .NET
            Software tools: Familiar with the basics of the unix command line and git. 
            Engineering tools: Some exposure and experience with LabVIEW and MATLAB from school final year projects.
            Misc: Soldering, reading documentation and legacy code
            <br>
            Personal Projects
            <br>
            Matasano Crypto Challenges - Learning exercise in C.
            Web stuff - Learning exercise in web stuff in Clojure/Clojurescript.
            Kossel 3D printer - Learning exercise in hands on hardware.
            <br>
            I like reading and the idea of running.
          </p>
        </div>
      </div>
    </div>

    <!-- End Document
         –––––––––––––––––––––––––––––––––––––––––––––––––– -->
  </body>
</html>
