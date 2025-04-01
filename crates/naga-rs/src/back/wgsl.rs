use crate::util::call_site;
use naga::{
    Expression, Literal, Module, Scalar, ScalarKind, Type, TypeInner,
    back::wgsl::{self, WriterFlags},
    valid::{Capabilities, ValidationFlags, Validator},
};
use syn::{Error, Result};

pub fn write_string(module: &mut Module, capability: Capabilities) -> Result<()> {
    // Resolves abstract numerics according to the capability.
    let int_width = 4 + capability.intersects(Capabilities::SHADER_INT64) as u8 * 4;
    let float_width = 4 + capability.intersects(Capabilities::FLOAT64) as u8 * 4;
    resolve_abstract(module, int_width, float_width)?;

    println!("@@@ module");
    println!("{:?}", module);

    // Validates the naga module.
    println!("@@@ validation begins");
    let mut validator = Validator::new(ValidationFlags::all(), capability);

    let info = validator
        .validate(module)
        .map_err(|e| Error::new(call_site(), e))?;

    if let Ok(res) = wgsl::write_string(module, &info, WriterFlags::empty()) {
        println!("@@@ === gen wgsl ===");
        println!("{res}");
    };

    Ok(())
}

/// Resolves abstract numerics to i32/i64 or f32/f64.
///
/// - Type -> Scalar
/// - Expression -> Literal
pub fn resolve_abstract(module: &mut Module, int_width: u8, float_width: u8) -> Result<()> {
    // Resolves types.
    let mut handles = Vec::new();
    for (handle, ty) in module.types.iter() {
        if matches!(ty.inner, TypeInner::Scalar(v) if v.is_abstract()) {
            handles.push(handle);
        }
    }
    for handle in handles {
        let ty = &module.types[handle];
        let TypeInner::Scalar(v) = &ty.inner else {
            unreachable!()
        };
        match v.kind {
            ScalarKind::AbstractInt => {
                module.types.replace(
                    handle,
                    Type {
                        name: ty.name.clone(),
                        inner: TypeInner::Scalar(Scalar {
                            kind: ScalarKind::Sint,
                            width: int_width,
                        }),
                    },
                );
            }
            ScalarKind::AbstractFloat => {
                module.types.replace(
                    handle,
                    Type {
                        name: ty.name.clone(),
                        inner: TypeInner::Scalar(Scalar {
                            kind: ScalarKind::Float,
                            width: float_width,
                        }),
                    },
                );
            }
            _ => unreachable!(),
        }
    }

    // Resolves global expressions.
    for (_, expr) in module.global_expressions.iter_mut() {
        resolve_expr(expr, int_width, float_width)?;
    }

    // Resolves local expressions.
    for (_, function) in module.functions.iter_mut() {
        for (_, expr) in function.expressions.iter_mut() {
            resolve_expr(expr, int_width, float_width)?;
        }
    }

    return Ok(());

    // === Internal helper functions ===

    fn resolve_expr(expr: &mut Expression, int_width: u8, float_width: u8) -> Result<()> {
        let Expression::Literal(lit) = expr else {
            return Ok(());
        };
        match lit {
            Literal::AbstractInt(v) => {
                if int_width == 4 {
                    if *v > i32::MAX as i64 {
                        return Err(Error::new(call_site(), &format!("`{v}` exceeds i32::MAX")));
                    }
                    *lit = Literal::I32(*v as _);
                } else {
                    *lit = Literal::I64(*v);
                }
            }
            Literal::AbstractFloat(v) => {
                if float_width == 4 {
                    *lit = Literal::F32(*v as _);
                } else {
                    *lit = Literal::F64(*v);
                }
            }
            _ => {}
        }
        Ok(())
    }
}
