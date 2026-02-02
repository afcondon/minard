use serde_json::Value;

/// Render a PureScript type AST to a human-readable string
pub fn render_type(type_ast: &Value) -> String {
    render_type_inner(type_ast, false)
}

fn render_type_inner(type_ast: &Value, in_app_right: bool) -> String {
    let tag = type_ast.get("tag").and_then(|t| t.as_str()).unwrap_or("");

    match tag {
        "TypeVar" => type_ast
            .get("contents")
            .and_then(|c| c.as_str())
            .unwrap_or("?")
            .to_string(),

        "TypeConstructor" => render_type_constructor(type_ast),

        "TypeApp" => render_type_app(type_ast, in_app_right),

        "ForAll" => render_forall(type_ast),

        "ConstrainedType" => render_constrained(type_ast),

        "ParensInType" => {
            let inner = type_ast
                .get("contents")
                .map(|c| render_type_inner(c, false))
                .unwrap_or_default();
            format!("({})", inner)
        }

        "REmpty" => "()".to_string(),

        "RCons" => render_row(type_ast),

        "TypeWildcard" => "_".to_string(),

        "TypeLevelString" => {
            let s = type_ast
                .get("contents")
                .and_then(|c| c.as_str())
                .unwrap_or("");
            format!("\"{}\"", s)
        }

        "TypeLevelInt" => type_ast
            .get("contents")
            .map(|c| c.to_string())
            .unwrap_or_default(),

        "TypeOp" => render_type_op(type_ast),

        "BinaryNoParensType" => render_binary_type(type_ast),

        "KindedType" => {
            let contents = type_ast.get("contents");
            if let Some(arr) = contents.and_then(|c| c.as_array()) {
                if arr.len() >= 2 {
                    let ty = render_type_inner(&arr[0], false);
                    let kind = render_type_inner(&arr[1], false);
                    return format!("({} :: {})", ty, kind);
                }
            }
            "?kinded".to_string()
        }

        _ => {
            // Fallback: try to extract something useful
            if let Some(contents) = type_ast.get("contents") {
                if contents.is_string() {
                    return contents.as_str().unwrap_or("?").to_string();
                }
            }
            format!("?{}", tag)
        }
    }
}

fn render_type_constructor(type_ast: &Value) -> String {
    let contents = type_ast.get("contents");
    if let Some(arr) = contents.and_then(|c| c.as_array()) {
        // Contents is [moduleName, typeName]
        if arr.len() >= 2 {
            if let Some(type_name) = arr[1].as_str() {
                // Special case: Prim.Function -> (->)
                if let Some(mod_arr) = arr[0].as_array() {
                    let mod_parts: Vec<&str> =
                        mod_arr.iter().filter_map(|v| v.as_str()).collect();
                    if mod_parts == ["Prim"] && type_name == "Function" {
                        return "(->)".to_string();
                    }
                    // Optionally include module for qualified names
                    // For now, just return the type name
                }
                return type_name.to_string();
            }
        }
    }
    "?constructor".to_string()
}

fn render_type_app(type_ast: &Value, in_app_right: bool) -> String {
    let contents = type_ast.get("contents");
    if let Some(arr) = contents.and_then(|c| c.as_array()) {
        if arr.len() >= 2 {
            let left = &arr[0];
            let right = &arr[1];

            // Special case: Function application (a -> b)
            if is_function(left) {
                let l = render_type_inner(right, false);
                // Look for nested function to get the return type
                if let Some(ret_arr) = left.get("contents").and_then(|c| c.as_array()) {
                    if ret_arr.len() >= 2 {
                        let r = render_type_inner(&ret_arr[1], true);
                        return format!("{} -> {}", l, r);
                    }
                }
            }

            // Check if this is the right side of a function app
            if is_function_right_side(type_ast) {
                let l = render_type_inner(left, false);
                let r = render_type_inner(right, true);
                return format!("{} -> {}", l, r);
            }

            let l = render_type_inner(left, false);
            let r = render_type_inner(right, true);

            // If right side needs parens (is itself an app)
            if needs_parens(right) && in_app_right {
                return format!("{} ({})", l, r);
            }

            // Check if left is just Function, making this a partial application
            let left_tag = left.get("tag").and_then(|t| t.as_str()).unwrap_or("");
            if left_tag == "TypeConstructor" {
                if let Some(arr) = left.get("contents").and_then(|c| c.as_array()) {
                    if arr.len() >= 2 {
                        if let Some(mod_arr) = arr[0].as_array() {
                            let mod_parts: Vec<&str> =
                                mod_arr.iter().filter_map(|v| v.as_str()).collect();
                            if let Some(name) = arr[1].as_str() {
                                if mod_parts == ["Prim"] && name == "Function" {
                                    // This is "Function a" - partial application
                                    return format!("({} -> _)", r);
                                }
                            }
                        }
                    }
                }
            }

            return format!("{} {}", l, r);
        }
    }
    "?app".to_string()
}

fn is_function(type_ast: &Value) -> bool {
    let tag = type_ast.get("tag").and_then(|t| t.as_str()).unwrap_or("");
    if tag == "TypeApp" {
        if let Some(arr) = type_ast.get("contents").and_then(|c| c.as_array()) {
            if !arr.is_empty() {
                return is_function_constructor(&arr[0]);
            }
        }
    }
    false
}

fn is_function_constructor(type_ast: &Value) -> bool {
    let tag = type_ast.get("tag").and_then(|t| t.as_str()).unwrap_or("");
    if tag == "TypeConstructor" {
        if let Some(arr) = type_ast.get("contents").and_then(|c| c.as_array()) {
            if arr.len() >= 2 {
                if let Some(mod_arr) = arr[0].as_array() {
                    let mod_parts: Vec<&str> = mod_arr.iter().filter_map(|v| v.as_str()).collect();
                    if let Some(name) = arr[1].as_str() {
                        return mod_parts == ["Prim"] && name == "Function";
                    }
                }
            }
        }
    }
    false
}

fn is_function_right_side(type_ast: &Value) -> bool {
    let tag = type_ast.get("tag").and_then(|t| t.as_str()).unwrap_or("");
    if tag == "TypeApp" {
        if let Some(arr) = type_ast.get("contents").and_then(|c| c.as_array()) {
            if !arr.is_empty() {
                return is_function(&arr[0]);
            }
        }
    }
    false
}

fn needs_parens(type_ast: &Value) -> bool {
    let tag = type_ast.get("tag").and_then(|t| t.as_str()).unwrap_or("");
    matches!(tag, "TypeApp" | "ForAll" | "ConstrainedType")
}

fn render_forall(type_ast: &Value) -> String {
    let contents = type_ast.get("contents");
    if let Some(obj) = contents {
        // New format: { identifier, kind, skolem, type, visibility }
        if let Some(ident) = obj.get("identifier").and_then(|i| i.as_str()) {
            let inner_type = obj.get("type").map(|t| render_type_inner(t, false));
            if let Some(inner) = inner_type {
                // Check visibility - invisible quantifiers are implicit
                let visibility = obj
                    .get("visibility")
                    .and_then(|v| v.as_str())
                    .unwrap_or("");
                if visibility == "TypeVarInvisible" {
                    // Collect all chained foralls
                    let mut vars = vec![ident.to_string()];
                    let mut current = obj.get("type");

                    while let Some(t) = current {
                        if t.get("tag").and_then(|t| t.as_str()) == Some("ForAll") {
                            if let Some(inner_obj) = t.get("contents") {
                                if let Some(inner_ident) =
                                    inner_obj.get("identifier").and_then(|i| i.as_str())
                                {
                                    let inner_vis = inner_obj
                                        .get("visibility")
                                        .and_then(|v| v.as_str())
                                        .unwrap_or("");
                                    if inner_vis == "TypeVarInvisible" {
                                        vars.push(inner_ident.to_string());
                                        current = inner_obj.get("type");
                                        continue;
                                    }
                                }
                            }
                        }
                        break;
                    }

                    // Render the final inner type
                    let final_type = current.map(|t| render_type_inner(t, false)).unwrap_or(inner);
                    return format!("forall {}. {}", vars.join(" "), final_type);
                }
            }
        }
    }
    "?forall".to_string()
}

fn render_constrained(type_ast: &Value) -> String {
    let contents = type_ast.get("contents");
    if let Some(arr) = contents.and_then(|c| c.as_array()) {
        if arr.len() >= 2 {
            let constraint = render_constraint(&arr[0]);
            let inner = render_type_inner(&arr[1], false);
            return format!("{} => {}", constraint, inner);
        }
    }
    "?constrained".to_string()
}

fn render_constraint(constraint: &Value) -> String {
    let class_name = constraint
        .get("constraintClass")
        .and_then(|c| c.as_array())
        .and_then(|arr| {
            // constraintClass is [["Module", "Name"], "ClassName"]
            if arr.len() >= 2 {
                arr[1].as_str().map(|s| s.to_string())
            } else {
                None
            }
        })
        .unwrap_or_else(|| "?class".to_string());

    let args = constraint
        .get("constraintArgs")
        .and_then(|a| a.as_array())
        .map(|arr| {
            arr.iter()
                .map(|a| render_type_inner(a, false))
                .collect::<Vec<_>>()
                .join(" ")
        })
        .unwrap_or_default();

    if args.is_empty() {
        class_name
    } else {
        format!("{} {}", class_name, args)
    }
}

fn render_row(type_ast: &Value) -> String {
    let mut fields = Vec::new();
    let mut current = type_ast;

    loop {
        let tag = current.get("tag").and_then(|t| t.as_str()).unwrap_or("");
        match tag {
            "RCons" => {
                if let Some(arr) = current.get("contents").and_then(|c| c.as_array()) {
                    if arr.len() >= 3 {
                        let label = arr[0].as_str().unwrap_or("?");
                        let ty = render_type_inner(&arr[1], false);
                        fields.push(format!("{} :: {}", label, ty));
                        current = &arr[2];
                        continue;
                    }
                }
                break;
            }
            "REmpty" => break,
            _ => {
                // Row tail
                fields.push(format!("| {}", render_type_inner(current, false)));
                break;
            }
        }
    }

    format!("( {} )", fields.join(", "))
}

fn render_type_op(type_ast: &Value) -> String {
    let contents = type_ast.get("contents");
    if let Some(arr) = contents.and_then(|c| c.as_array()) {
        // Contents is [moduleName, opName]
        if arr.len() >= 2 {
            if let Some(op_name) = arr[1].as_str() {
                return op_name.to_string();
            }
        }
    }
    "?op".to_string()
}

fn render_binary_type(type_ast: &Value) -> String {
    let contents = type_ast.get("contents");
    if let Some(arr) = contents.and_then(|c| c.as_array()) {
        if arr.len() >= 3 {
            let op = render_type_inner(&arr[0], false);
            let left = render_type_inner(&arr[1], false);
            let right = render_type_inner(&arr[2], false);
            return format!("{} {} {}", left, op, right);
        }
    }
    "?binary".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_render_simple_types() {
        let type_var: Value = serde_json::json!({
            "tag": "TypeVar",
            "contents": "a"
        });
        assert_eq!(render_type(&type_var), "a");

        let type_constructor: Value = serde_json::json!({
            "tag": "TypeConstructor",
            "contents": [["Data", "Maybe"], "Maybe"]
        });
        assert_eq!(render_type(&type_constructor), "Maybe");
    }
}
