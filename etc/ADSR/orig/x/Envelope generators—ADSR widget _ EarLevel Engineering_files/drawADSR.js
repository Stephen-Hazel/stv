jQuery(document).ready(function( $ ) {
	envADSR = new ADSR;
	document.getElementById("ratioASlider").value = Math.log(0.5*1000+1) * 200.0 / 12.0;
	document.getElementById("ratioDRSlider").value = Math.log(0.0001*1000+1) * 200.0 / 12.0;

	drawAllADSR();
});

function drawAllADSR() {
	envADSR.setAttackRate(document.getElementById("attackSlider").value);
	envADSR.setDecayRate(document.getElementById("decaySlider").value);
	envADSR.setSustainLevel(document.getElementById("sustainSlider").value/200);
	envADSR.setReleaseRate(document.getElementById("releaseSlider").value);
	envADSR.setTargetRatioA(0.001 * (Math.exp(12.0*document.getElementById("ratioASlider").value/200)-1.0));
	envADSR.setTargetRatioDR(0.001 * (Math.exp(12.0*document.getElementById("ratioDRSlider").value/200)-1.0));
	drawADSR();
}

function drawADSR() {
	var val;
	var envPlot = [];
	envADSR.reset();
	envADSR.gate(1);
	envPlot.push([0, 0]);
	var idx;
	for (idx = 1; idx < 400; idx++)
		envPlot.push([idx, envADSR.process()]);
	envADSR.gate(0);
	for (idx = 400; idx < 600; idx++)
		envPlot.push([idx, envADSR.process()]);
	
	// plot linear or log
	if (document.getElementById("PlotType").value == "linear")
		Flotr.draw(document.getElementById('container20130623'), [ envPlot ], { yaxis: { max: 1.0, min: 0} });
	else {
		for (idx = 0; idx < 600; idx++) {
			val = envPlot[idx][1];
			if (val == 0)
				envPlot[idx][1] = -200;
			else
				envPlot[idx][1] = 20 * Math.log(val)/Math.LN10;
		}
		Flotr.draw(document.getElementById('container20130623'), [ envPlot ], { yaxis: { max: 0.0, min: -100} });
	}

}
