use std::io;
use std::fs::File;
use std::io::Read;
use std::process::Command;
use std::str;

extern crate bit_vec;
use bit_vec::BitVec;

fn main() -> io::Result<()> {
    print!("{}[2J", 27 as char);
    println!("Velkomin í Hengimann,");
    println!("leikinn sem gengur út á að hengja mann!");
    println!("Vinsamlegast sláðu inn einn staf");

    let pics = match read_data() { 
        Ok(t) => t,
        Err(..) => vec![String::from("error")],
    };

    let mut pics_iter = pics.iter();

    let word = get_random_word();

    let word_len = word.chars().count();
    let mut found_letters: BitVec = BitVec::from_elem(word_len, false);
    let mut already_guessed: Vec<char> = Vec::new();
    let mut current_pic: String = String::new();
    let mut message_line = String::new();
    
    loop {
        let mut buffer = String::new();

        print_status(&current_pic, &already_guessed, &word, 
                     &found_letters, &message_line);

        let stdin = io::stdin();
        stdin.read_line(&mut buffer)?;

        if buffer.chars().count() != 2 {
            message_line = "vinsamlegast sláðið þér inn einn staf".to_string();
            continue;
        }
        let tried_letter = buffer.remove(0);
        if already_guessed.contains(&tried_letter) {
            message_line = "þú hefur nú þegar prófað þennan eva".to_string();
            continue;
        }

        message_line = String::new();

        already_guessed.push(tried_letter);

        let does_match = found_letters.or(&marker(&word, tried_letter));
        if does_match {
            if found_letters.all() {
                print_status(&current_pic, &already_guessed, &word, 
                             &found_letters, &"sigur!".to_string());
                break;
            }
        } else {
            match pics_iter.next() {
                Some(pic) => {
                    current_pic = pic.to_string();
                },
                None => {
                    print_status(&current_pic, &already_guessed, &word, 
                                 &found_letters, &format!("tap!\norðið var {}", word));
                    break;
                },
            }
        }
    }
    Ok(())
}

fn print_status(current_pic: &String, 
                already_guessed: &Vec<char>, 
                word: &String, 
                found_letters: &BitVec,
                message_line: &String) {
        print!("{}[2J", 27 as char);
        println!("{}", current_pic);
        println!("{}", already_guessed_line(&already_guessed));
        println!("{}", correct_guesses_line(&word, &found_letters));
        println!("{}", message_line);
}

fn marker(word: &String, letter: char) -> BitVec {
    let mut marked: BitVec = BitVec::new();
    for l in word.chars() {
        marked.push(l == letter)
    }
    marked
}

fn read_data() -> io::Result<Vec<String>> {
    let mut file = File::open("hengimann.data")?;
    let mut contents = String::new();
    let mut pics: Vec<String> = Vec::new();
    pics.push(String::new());

    file.read_to_string(&mut contents)?;
    let lines = contents.lines();

    let mut i: usize = 0;

    for line in lines {
        if line == "===" {
            i += 1;
            pics.push(String::new());
            continue;
        }
        pics[i].push_str(line);
        pics[i].push_str("\n");
        
    }
    Ok(pics)
}

fn correct_guesses_line(word: &String, found_letters: &BitVec) -> String {
    let mut ret = String::new();
    let mut siter = word.chars();

    for b in found_letters {
        match siter.next() {
            Some(c) => {
                if b {
                    ret.push(c);
                } else {
                    ret.push('_');
                }
            },
            None => ()
        }
    }
    ret
}

fn already_guessed_line(xs: &Vec<char>) -> String {
    let mut ret = String::new();
    for x in xs {
        ret.push(*x);
        ret.push(' ');
    }
    ret
}

fn get_random_word() -> String {
       let raw = Command::new("shuf")
            .arg("-n")
            .arg("1")
            .arg("/usr/share/hunspell/is_IS.dic")
            .output()
            .expect("failed to execute process").stdout;
    return str::from_utf8(&raw).unwrap().trim_matches(|c: char| -> bool {!char::is_alphabetic(c)}).to_string();
}
