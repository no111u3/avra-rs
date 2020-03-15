//! Contains device specific options of AVRA-rs

use lazy_static::lazy_static;
use maplit::{btreeset, hashmap};

use std::collections::{BTreeSet, HashMap};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum DisabledOptions {
    NoMul,
    /// No JMP, CALL
    NoJmp,
    /// No X register
    NoXreg,
    /// No Y register
    NoYreg,
    /// AT90S1200, ATtiny10-12, no ADIW, SBIW, IJMP, ICALL, LDD, STD, LDS, STS, PUSH, POP
    Tiny1x,
    /// No LPM instruction
    NoLpm,
    /// No LPM Rd, Z or LPM Rd, Z+ instruction
    NoLpmX,
    /// No ELPM instruction
    NoElpm,
    /// No ELPM Rd, Z or ELPM Rd, Z+ instruction
    NoElpmX,
    /// No SPM instruction
    NoSpm,
    /// No ESPM instruction
    NoEspm,
    /// No MOVW instruction
    NoMovw,
    /// No BREAK instruction
    NoBreak,
    /// No EICALL instruction
    NoEicall,
    /// No EIJMP instruction
    NoEijmp,
    /// ATtiny10, 20, 40 no ADIW, SBIW, one word LDS/STS
    Avr8l,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Device {
    /// Flash size in words
    pub flash_size: u32,
    pub ram_start: u32,
    pub ram_size: u32,
    pub eeprom_size: u32,
    pub disable_opts: BTreeSet<DisabledOptions>,
}

use DisabledOptions::*;

use crate::instruction::operation::Operation;

impl Device {
    pub fn new(flash_size: u32) -> Self {
        let flash_size = if flash_size == 0 { 4194304 } else { flash_size };
        Self {
            flash_size,
            ram_start: 0x60,
            ram_size: 8388608,
            eeprom_size: 65536,
            disable_opts: btreeset! {},
        }
    }

    pub fn check_operation(&self, op: &Operation) -> bool {
        match op {
            Operation::Mul => self.allow(NoMul),
            Operation::Jmp => self.allow(NoJmp),
            Operation::Lpm => self.allow(NoLpm),
            Operation::Elpm => self.allow(NoElpm),
            Operation::Spm => self.allow(NoSpm),
            Operation::Eicall => self.allow(NoEicall),
            Operation::Eijmp => self.allow(NoEijmp),
            Operation::Break => self.allow(NoBreak),
            Operation::Movw => self.allow(NoMovw),
            // AT90S1200, ATtiny10-12, no ADIW, SBIW, IJMP, ICALL, LDD, STD, LDS, STS, PUSH, POP
            Operation::Adiw
            | Operation::Sbiw
            | Operation::Ijmp
            | Operation::Icall
            | Operation::Ldd
            | Operation::Std
            | Operation::Lds
            | Operation::Sts
            | Operation::Push
            | Operation::Pop => {
                if self.allow(Tiny1x) {
                    // ATtiny10, 20, 40 no ADIW, SBIW, one word LDS/STS
                    match op {
                        Operation::Adiw | Operation::Sbiw => self.allow(Avr8l),
                        _ => true,
                    }
                } else {
                    false
                }
            }
            _ => true,
        }
    }

    pub fn is_avr8l(&self) -> bool {
        self.disable_opts.contains(&Avr8l)
    }

    pub fn allow(&self, o: DisabledOptions) -> bool {
        !self.disable_opts.contains(&o)
    }
}

lazy_static! {
pub static ref DEVICES: HashMap<&'static str, Device> = hashmap!{
/* Name => Flash(words), RAM start, RAM size, EEPROM, disable options */
  /* ATtiny Series */
  // ATtiny4
  // ATtiny5
  // ATtiny9
  "ATtiny10" => Device {flash_size: 512, ram_start: 0x00, ram_size: 0, eeprom_size: 0, disable_opts: btreeset!{NoMul, NoJmp, Tiny1x, NoXreg, NoYreg, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "ATtiny11" => Device {flash_size: 512, ram_start: 0x00, ram_size: 0, eeprom_size: 0, disable_opts: btreeset!{NoMul, NoJmp, Tiny1x, NoXreg, NoYreg, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "ATtiny12" => Device {flash_size: 512, ram_start: 0x00, ram_size: 0, eeprom_size: 64, disable_opts: btreeset!{NoMul, NoJmp, Tiny1x, NoXreg, NoYreg, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "ATtiny13" => Device {flash_size: 512, ram_start: 0x60, ram_size: 64, eeprom_size: 64, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny13A" => Device {flash_size: 512, ram_start: 0x60, ram_size: 64, eeprom_size: 64, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny15" => Device {flash_size: 512, ram_start: 0x00, ram_size: 0, eeprom_size: 64, disable_opts: btreeset!{NoMul, NoJmp, Tiny1x, NoXreg, NoYreg, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "ATtiny20" => Device {flash_size: 2048, ram_start: 0x40, ram_size: 128, eeprom_size: 0, disable_opts: btreeset!{Avr8l, NoJmp, NoMul, NoEijmp, NoEicall, NoMovw, NoLpm, NoElpm, NoSpm, NoEspm, NoBreak} },
  "ATtiny22" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "ATtiny24" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny24A" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny25" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny26" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "ATtiny28" => Device {flash_size: 1024, ram_start: 0x00, ram_size: 0, eeprom_size: 0, disable_opts: btreeset!{NoMul, NoJmp, Tiny1x, NoXreg, NoYreg, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  // ATtiny43U
  "ATtiny44" => Device {flash_size: 2048, ram_start: 0x60, ram_size: 256, eeprom_size: 256, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny44A" => Device {flash_size: 2048, ram_start: 0x60, ram_size: 256, eeprom_size: 256, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny45" => Device {flash_size: 2048, ram_start: 0x60, ram_size: 256, eeprom_size: 256, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny48" => Device {flash_size: 2048, ram_start: 0x100, ram_size: 256, eeprom_size: 64, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny84" => Device {flash_size: 4096, ram_start: 0x60, ram_size: 512, eeprom_size: 512, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny85" => Device {flash_size: 4096, ram_start: 0x60, ram_size: 512, eeprom_size: 512, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny88" => Device {flash_size: 4096, ram_start: 0x100, ram_size: 512, eeprom_size: 64, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  // ATtiny87
  // ATtiny167
  // ATtiny261A
  // ATtiny461A
  // ATtiny861A
  "ATtiny2313" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny2313A" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  "ATtiny4313" => Device {flash_size: 2048, ram_start: 0x60, ram_size: 256, eeprom_size: 256, disable_opts: btreeset!{NoMul, NoJmp, NoElpm, NoEspm, NoEicall, NoEijmp} },
  /* AT90 series */
  "AT90S1200" => Device {flash_size: 512, ram_start: 0x00, ram_size: 0, eeprom_size: 64, disable_opts: btreeset!{NoMul, NoJmp, Tiny1x, NoXreg, NoYreg, NoLpm, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} }, // 137 - MUL(6) - JMP(2) - TINY(10)
  "AT90S2313" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "AT90S2323" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "AT90S2333" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "AT90S2343" => Device {flash_size: 1024, ram_start: 0x60, ram_size: 128, eeprom_size: 128, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "AT90S4414" => Device {flash_size: 2048, ram_start: 0x60, ram_size: 256, eeprom_size: 256, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "AT90S4433" => Device {flash_size: 2048, ram_start: 0x60, ram_size: 128, eeprom_size: 256, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "AT90S4434" => Device {flash_size: 2048, ram_start: 0x60, ram_size: 256, eeprom_size: 256, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "AT90S8515" => Device {flash_size: 4096, ram_start: 0x60, ram_size: 512, eeprom_size: 512, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} }, // 137 - MUL(6) - JMP(2) - LPM_X(2) - ELPM(3) - SPM - ESPM - MOVW - BREAK - EICALL - EIJMP = 118
  "AT90C8534" => Device {flash_size: 4096, ram_start: 0x60, ram_size: 256, eeprom_size: 512, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  "AT90S8535" => Device {flash_size: 4096, ram_start: 0x60, ram_size: 512, eeprom_size: 512, disable_opts: btreeset!{NoMul, NoJmp, NoLpmX, NoElpm, NoSpm, NoEspm, NoMovw, NoBreak, NoEicall, NoEijmp} },
  /* AT90USB series*/
  // AT90USB168
  // AT90USB1287
  /* ATmega series */
  "ATmega8" => Device {flash_size: 4096, ram_start: 0x60, ram_size: 1024, eeprom_size: 512, disable_opts: btreeset!{NoJmp, NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega161" => Device {flash_size: 8192, ram_start: 0x60, ram_size: 1024, eeprom_size: 512, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega162" => Device {flash_size: 8192, ram_start: 0x100, ram_size: 1024, eeprom_size: 512, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega163" => Device {flash_size: 8192, ram_start: 0x60, ram_size: 1024, eeprom_size: 512, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega16" => Device {flash_size: 8192, ram_start: 0x60, ram_size: 1024, eeprom_size: 512, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega323" => Device {flash_size: 16384, ram_start: 0x60, ram_size: 2048, eeprom_size: 1024, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} }, // 137 - EICALL - EIJMP - ELPM(3) - ESPM = 131 (Data sheet says 130 but it's wrong)
  "ATmega328P" => Device {flash_size: 16384, ram_start: 0x100, ram_size: 2048, eeprom_size: 1024, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega32" => Device {flash_size: 16384, ram_start: 0x60, ram_size: 2048, eeprom_size: 1024, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega603" => Device {flash_size: 32768, ram_start: 0x60, ram_size: 4096, eeprom_size: 2048, disable_opts: btreeset!{NoEicall, NoEijmp, NoMul, NoMovw, NoLpmX, NoElpm, NoSpm, NoEspm, NoBreak} },
  "ATmega103" => Device {flash_size: 65536, ram_start: 0x60, ram_size: 4096, eeprom_size: 4096, disable_opts: btreeset!{NoEicall, NoEijmp, NoMul, NoMovw, NoLpmX, NoElpmX, NoSpm, NoEspm, NoBreak} }, // 137 - EICALL - EIJMP - MUL(6) - MOVW - LPM_X(2) - ELPM_X(2) - SPM - ESPM - BREAK = 121
  "ATmega104" => Device {flash_size: 65536, ram_start: 0x60, ram_size: 4096, eeprom_size: 4096, disable_opts: btreeset!{NoEicall, NoEijmp, NoEspm} }, // Old name for mega128
  "ATmega128" => Device {flash_size: 65536, ram_start: 0x100, ram_size: 4096, eeprom_size: 4096, disable_opts: btreeset!{NoEicall, NoEijmp, NoEspm} }, // 137 - EICALL - EIJMP - ESPM = 134 (Data sheet says 133 but it's wrong)
  "ATmega48" => Device {flash_size: 2048, ram_start: 0x100, ram_size: 512, eeprom_size: 256, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega88" => Device {flash_size: 4096, ram_start: 0x100, ram_size: 1024, eeprom_size: 512, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega168" => Device {flash_size: 8192, ram_start: 0x100, ram_size: 1024, eeprom_size: 512, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega644" => Device {flash_size: 65536, ram_start: 0x100, ram_size: 4096, eeprom_size: 2048, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega8515" => Device {flash_size: 8192, ram_start: 0x60, ram_size: 512, eeprom_size: 512, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoEspm} },
  "ATmega1280" => Device {flash_size: 65536, ram_start: 0x200, ram_size: 8192, eeprom_size: 4096, disable_opts: btreeset!{NoEicall, NoEijmp, NoEspm} },
  "ATmega2560" => Device {flash_size: 262144, ram_start: 0x200, ram_size: 8192, eeprom_size: 4096, disable_opts: btreeset!{NoEspm} },
  /* Other */
  "AT94K" => Device {flash_size: 8192, ram_start: 0x60, ram_size: 16384, eeprom_size: 0, disable_opts: btreeset!{NoEicall, NoEijmp, NoElpm, NoSpm, NoEspm, NoBreak} }, // 137 - EICALL - EIJMP - ELPM(3) - SPM - ESPM - BREAK = 129
};
}
