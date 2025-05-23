/**
 * @author Nigel Redmon
 */


function ADSR() {
    this.envState = {
        "env_idle": 0,
        "env_attack" : 1,
        "env_decay" : 2,
        "env_sustain" : 3,
        "env_release" : 4
    };

    this.state = this.envState.env_idle;
    this.output = 0.0;
    this.attackRate = 0;
    this.decayRate = 0;
    this.releaseRate = 0;
    this.attackCoef = 0.0;
    this.decayCoef = 0.0;
    this.releaseCoef = 0.0;
    this.sustainLevel = 1.0;
    this.targetRatioA = 0.01;
    this.targetRatioDR = 0.0001;
    this.attackBase = (1.0 + this.targetRatioA) * (1.0 - this.attackCoef);
    this.decayBase = (this.sustainLevel - this.targetRatioDR) * (1.0 - this.decayCoef);
    this.releaseBase = -this.targetRatioDR * (1.0 - this.releaseCoef);

    this.process = function () {
        switch (this.state) {
            case this.envState.env_idle:
                break;
            case this.envState.env_attack:
                this.output = this.attackBase + this.output * this.attackCoef;
                if (this.output >= 1.0) {
                    this.output = 1.0;
                    this.state = this.envState.env_decay;
                }
                break;
            case this.envState.env_decay:
                this.output = this.decayBase + this.output * this.decayCoef;
                if (this.output <= this.sustainLevel) {
                    this.output = this.sustainLevel;
                    this.state = this.envState.env_sustain;
                }
                break;
            case this.envState.env_sustain:
                break;
            case this.envState.env_release:
                this.output = this.releaseBase + this.output * this.releaseCoef;
                if (this.output <= 0.0) {
                    this.output = 0.0;
                    this.state = this.envState.env_idle;
                }
                break;
        }
        return this.output;

    }

    this.gate = function (gate) {
        if (gate > 0)
            this.state = this.envState.env_attack;
        else {
            if (this.state != this.envState.env_idle)
                this.state = this.envState.env_release;
        }
    }

    this.setAttackRate = function (rate) {
        this.attackRate = rate;
        this.attackCoef = this.calcCoef(rate, this.targetRatioA);
        this.attackBase = (1.0 + this.targetRatioA) * (1.0 - this.attackCoef);
    }

    this.setDecayRate = function (rate) {
        this.decayRate = rate;
        this.decayCoef = this.calcCoef(rate, this.targetRatioDR);
        this.decayBase = (this.sustainLevel - this.targetRatioDR) * (1.0 - this.decayCoef);
    }

    this.setReleaseRate = function (rate) {
        this.releaseRate = rate;
        this.releaseCoef = this.calcCoef(rate, this.targetRatioDR);
        this.releaseBase = -this.targetRatioDR * (1.0 - this.releaseCoef);
    }

    this.calcCoef = function (rate, targetRatio) {
        return Math.exp(-Math.log((1.0 + targetRatio) / targetRatio) / rate);
    }

    this.setSustainLevel = function (level) {
        this.sustainLevel = level;
        this.decayBase = (this.sustainLevel - this.targetRatioDR) * (1.0 - this.decayCoef);
    }

    this.setTargetRatioA = function (targetRatio) {
        if (targetRatio < 0.000000001)
            targetRatio = 0.000000001;  // -180 dB
            this.targetRatioA = targetRatio;
        this.attackCoef = this.calcCoef(this.attackRate, this.targetRatioA);
        this.attackBase = (1.0 + this.targetRatioA) * (1.0 - this.attackCoef);
    }

    this.setTargetRatioDR = function (targetRatio) {
        if (targetRatio < 0.000000001)
            targetRatio = 0.000000001;  // -180 dB
            this.targetRatioDR = targetRatio;
        this.decayCoef = this.calcCoef(this.decayRate, targetRatio);
        this.releaseCoef = this.calcCoef(this.releaseRate, targetRatio);
        this.decayBase = (this.sustainLevel - this.targetRatioDR) * (1.0 - this.decayCoef);
        this.releaseBase = -this.targetRatioDR * (1.0 - this.releaseCoef);
    }

    this.getOutput = function () {
        return this.output;
    }

    this.reset = function () {
        this.state = this.envState.idle;
        this.output = 0.0;
    }
}




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
