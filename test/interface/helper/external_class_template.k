// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.external_class_template { module helper.external_class_template }

template<uint16 Mask>
class Inverter
{
public:
    [[latency(1)]] uint16 Invert(uint16 a);
}

extern Inverter;


template<auto Width, auto C0, auto C1 = 0, auto Operation = "Add">
class Computer
{
public:
    uint<Width> Exec(uint<Width> a);

    [[latency(1)]] uint<Width> ExecFixed(uint<Width> a);
}

// There is no Verilog module named Computer
// But there is one named AluTemplate
[[name("AluTemplate")]]
extern Computer;