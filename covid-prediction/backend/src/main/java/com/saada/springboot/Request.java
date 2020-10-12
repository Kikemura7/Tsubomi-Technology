package com.saada.springboot;

public class Request {
    Double systolicBP;
    Double diastolicBP;
    Double age;
    Double bun;
    Double ldh;
    Double pulseOx;
    Double glucose;
    Double rr;
    Double troponin;
    Double ddimer;
    public void setSystolicBP(String value) {
        this.systolicBP = Double.valueOf(value);
    }
    public void setDiastolicBP(String value) {
        this.diastolicBP = Double.valueOf(value);
    }
    public void setAge(String value) {
        this.age = Double.valueOf(value);
    }
    public void setBun(String value) {
        this.bun = Double.valueOf(value);
    }
    public void setLdh(String value) {
        this.ldh = Double.valueOf(value);
    }
    public void setPulseOx(String value) {
        this.pulseOx = Double.valueOf(value);
    }
    public void setGlucose(String value) {
        this.glucose = Double.valueOf(value);
    }
    public void setRr(String value) {
        this.rr = Double.valueOf(value);
    }
    public void setTroponin(String value) {
        this.troponin = Double.valueOf(value);
    }
    public void setDdimer(String value) {
        this.ddimer = Double.valueOf(value);
    }

    public String toString() {
        return "systolicBP: " + this.systolicBP + " ldh: " + this.ldh;
    }
}