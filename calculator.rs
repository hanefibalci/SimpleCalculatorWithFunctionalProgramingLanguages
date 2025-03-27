use std::collections::HashMap;
use std::io::{self, Write};

fn main() {
    let mut memory: HashMap<String, f64> = HashMap::new();
    
    println!("Rust Expression Evaluator. Type 'exit' to quit.");

    loop {
        print!("$ ");
        io::stdout().flush().unwrap();

        let mut user_input = String::new();
        if io::stdin().read_line(&mut user_input).is_err() {
            eprintln!("Failed to read input. Please try again.");
            continue;
        }
        
        let command = user_input.trim();
        if command.eq_ignore_ascii_case("exit") {
            break;
        }

        if command.is_empty() {
            continue;
        }

        match handle_command(command, &mut memory) {
            Ok(Some(result)) => println!("{}", result),
            Ok(None) => {}
            Err(error) => eprintln!("Error: {}", error),
        }
    }
}

fn handle_command(command: &str, memory: &mut HashMap<String, f64>) 
    -> Result<Option<f64>, String> 
{
    if let Some(assignment_pos) = command.find('=') {
        let identifier = command[..assignment_pos].trim();
        let expression = command[assignment_pos+1..].trim();

        if identifier.is_empty() {
            return Err("Invalid identifier name".into());
        }

        let computed_value = compute_expression(expression, memory)?;
        memory.insert(identifier.to_string(), computed_value);
        Ok(None)
    } else {
        let result = compute_expression(command, memory)?;
        Ok(Some(result))
    }
}

fn compute_expression(expr: &str, memory: &HashMap<String, f64>) -> Result<f64, String> {
    let tokens: Vec<&str> = expr.split_whitespace().collect();
    if tokens.is_empty() {
        return Err("Invalid expression".into());
    }

    let mut accumulator = parse_token(tokens[0], memory)?;

    let mut position = 1;
    while position < tokens.len() {
        let operator = tokens[position];
        if position + 1 >= tokens.len() {
            return Err("Incomplete expression".into());
        }
        let operand = parse_token(tokens[position+1], memory)?;
        
        accumulator = match operator {
            "+" => accumulator + operand,
            "-" => accumulator - operand,
            "*" => accumulator * operand,
            "/" => {
                if operand == 0.0 {
                    return Err("Division by zero error".into());
                }
                accumulator / operand
            }
            _ => return Err(format!("Unknown operator: {}", operator)),
        };
        position += 2;
    }
    
    Ok(accumulator)
}

fn parse_token(token: &str, memory: &HashMap<String, f64>) -> Result<f64, String> {
    if let Ok(numeric_value) = token.parse::<f64>() {
        Ok(numeric_value)
    } else {
        match memory.get(token) {
            Some(value) => Ok(*value),
            None => Err(format!("Undefined variable: {}", token)),
        }
    }
}
