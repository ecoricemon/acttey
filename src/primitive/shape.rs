use super::{
    constant::{color, radian},
    mesh::Mesh,
    transform::rotate_y,
    vector::Vector,
    Color, Normal, Position,
};

#[derive(Copy, Clone)]
enum Edge {
    F(usize, usize), // Forward(start index, length)
    R(usize, usize), // Reverse(start index of its forwarding edge, length)
}

impl Default for Edge {
    fn default() -> Self {
        Edge::F(0, 0)
    }
}

#[derive(Copy, Clone, Default)]
struct Face {
    v: [usize; 3],
    e: [Edge; 3],
}

/// Samples: "square", "circle", "cube", "sphere"
pub fn sample(name: &str) -> Mesh {
    match name {
        "square" => sample_square(),
        "circle" => sample_circle(),
        "cube" => sample_cube(),
        "sphere" => sample_icosphere(),
        _ => panic!(),
    }
}

fn sample_square() -> Mesh {
    create_square(
        [
            Position::new(-1.0, -1.0, 0.0),
            Position::new(1.0, -1.0, 0.0),
            Position::new(-1.0, 1.0, 0.0),
            Position::new(1.0, 1.0, 0.0),
        ],
        [color::BLUE, color::GREEN, color::MAGENTA, color::YELLOW],
    )
}

fn sample_circle() -> Mesh {
    create_circle([0.0, 0.0].into(), 1.0, 64, color::DEFAULT)
}

fn sample_cube() -> Mesh {
    create_cube(
        Vector::<f32, 3>::new(0.0, 0.0, -1.0),
        2.0,
        2.0,
        2.0,
        color::DEFAULT,
    )
}

fn sample_icosphere() -> Mesh {
    create_icosphere(0.5, 3, color::DEFAULT)
}

// BL, BR, TL, TR
pub fn create_square(positions: [Position; 4], colors: [Color; 4]) -> Mesh {
    let normal = Normal::from([0.0, 0.0, 1.0]);
    Mesh::new()
        .with_position(Vec::from(positions).into())
        .with_normal(vec![normal; 4].into())
        .with_color(Vec::from(colors).into())
        .with_indices(vec![0, 1, 2, 2, 1, 3])
}

pub fn create_circle(center: Vector<f32, 2>, radius: f32, vertices: u32, color: Color) -> Mesh {
    const DEFAULT: [f32; 4] = [0.0, 0.0, 0.0, 1.0];
    let n = vertices.max(3);
    let positions = (0..n)
        .map(|i| {
            let theta = radian::TAU / n as f32 * i as f32;
            let (s, c) = theta.sin_cos();
            Position::from_arr_f32([center.x() + radius * c, center.y() + radius * s], DEFAULT)
        })
        .collect::<Vec<_>>();

    // CCW, Triangle List
    let indices = (2..n).flat_map(|i| [0, i - 1, i]).collect();

    let normal = Normal::from([0.0, 0.0, 1.0]);

    Mesh::new()
        .with_position(positions.into())
        .with_normal(vec![normal; n as usize].into())
        .with_color(vec![color; n as usize].into())
        .with_indices(indices)
}

pub fn create_cube(
    center: Vector<f32, 3>,
    width: f32,
    height: f32,
    depth: f32,
    color: Color,
) -> Mesh {
    const DEFAULT: [f32; 4] = [0.0, 0.0, 0.0, 1.0];
    let (hw, hh, hd) = (width / 2.0, height / 2.0, depth / 2.0);
    let fbl = center + Vector::<f32, 3>::new(-hw, -hh, hd);
    let ref_positions: Vec<Position> = (0..8)
        .map(|i| {
            Position::from_vec_f32(
                fbl + Vector::<f32, 3>::new(
                    if i & 1 != 0 { width } else { 0.0 },
                    if i & 2 != 0 { height } else { 0.0 },
                    if i & 4 != 0 { -depth } else { 0.0 },
                ),
                DEFAULT,
            )
        })
        .collect();
    let ref_normals = [
        Normal::from_arr_f32([1.0, 0.0, 0.0], DEFAULT), // Right
        Normal::from_arr_f32([-1.0, 0.0, 0.0], DEFAULT), // Left
        Normal::from_arr_f32([0.0, 1.0, 0.0], DEFAULT), // Top
        Normal::from_arr_f32([0.0, -1.0, 0.0], DEFAULT), // Bottom
        Normal::from_arr_f32([0.0, 0.0, 1.0], DEFAULT), // Front
        Normal::from_arr_f32([0.0, 0.0, -1.0], DEFAULT), // Rear
    ];
    let planes: [[usize; 4]; 6] = [
        [1, 5, 3, 7],
        [4, 0, 6, 2],
        [2, 3, 6, 7],
        [1, 0, 5, 4],
        [0, 1, 2, 3],
        [5, 4, 7, 6],
    ];

    let mut positions = Vec::with_capacity(24);
    let mut normals = Vec::with_capacity(24);

    for (ni, plane) in planes.into_iter().enumerate() {
        for pi in plane {
            positions.push(ref_positions[pi]);
            normals.push(ref_normals[ni]);
        }
    }

    // CCW, Triangle List
    let indices = (0..6)
        .map(|i| i * 4)
        .flat_map(|i| [i, i + 1, i + 2, i + 2, i + 1, i + 3])
        .collect();

    Mesh::new()
        .with_position(positions.into())
        .with_normal(normals.into())
        .with_color(vec![color; 24].into())
        .with_indices(indices)
}

pub fn create_icosahedron(
    radius: f32,
    color: Color,
    vertex_cap: Option<usize>,
    index_cap: Option<usize>,
) -> Mesh {
    // Reference: https://en.wikipedia.org/wiki/Regular_icosahedron
    let a = 1_f32 / 5_f32.sqrt() * radius;
    let mut coords = Vec::with_capacity(vertex_cap.unwrap_or(12));
    coords.push(Position::new(0.0, 1.0 * radius, 0.0));
    coords.push(Position::new(2.0 * a, a, 0.0));
    for i in 1..=4 {
        coords.push(&rotate_y(radian::FRAC_TAU_5 * i as f32) * coords[1]);
    }
    let rot_mat = rotate_y(radian::FRAC_PI_5);
    for i in 1..=5 {
        coords.push(&rot_mat * (coords[i] - Position::new(0.0, 2.0 * a, 0.0)));
    }
    coords.push(Position::new(0.0, -1.0 * radius, 0.0));

    let positions: Vec<Position> = coords
        .into_iter()
        .map(|coord| Position::from_vec_f32(coord, [0.0, 0.0, 0.0, 1.0]))
        .collect();

    // CCW, Triangle List
    let mut indices = Vec::with_capacity(index_cap.unwrap_or(60));
    if index_cap.is_none() {
        indices.resize(60, 0);
        indices[..60].copy_from_slice(&[
            0, 1, 2, 1, 6, 2, 2, 6, 7, 7, 6, 11, 0, 2, 3, 2, 7, 3, 3, 7, 8, 8, 7, 11, 0, 3, 4, 3,
            8, 4, 4, 8, 9, 9, 8, 11, 0, 4, 5, 4, 9, 5, 5, 9, 10, 10, 9, 11, 0, 5, 1, 5, 10, 1, 1,
            10, 6, 6, 10, 11,
        ]);
    }

    let normals = positions.clone();
    let colors = vec![color; positions.len()];

    Mesh::new()
        .with_position(positions.into())
        .with_normal(normals.into())
        .with_color(colors.into())
        .with_indices(indices)
}

fn cut_arc_into_pow2(buf: &mut [Position], off: usize, len: usize, si: usize, ei: usize) {
    if len == 0 {
        return;
    }
    let half = len / 2;
    buf[off + half] = (buf[si] + buf[ei]).make_unit();
    cut_arc_into_pow2(buf, off, half, si, off + half);
    cut_arc_into_pow2(buf, off + half + 1, half, off + half, ei);
}

fn cut_icosahedron_edges(
    buf: &mut [Position],
    shared_off: usize,
    shared_unit: usize,
) -> [Face; 20] {
    // Set face info
    let mut faces = [Face::default(); 20];
    for (i, face) in faces.iter_mut().enumerate() {
        let (i2, i4) = (i >> 1, i >> 2);
        match i % 4 {
            0 => {
                face.v = [0, i4 + 1, (i4 + 1) % 5 + 1];
                face.e = [
                    Edge::F(shared_off + i2 * shared_unit, shared_unit),
                    Edge::F(shared_off + (i2 + 1) * shared_unit, shared_unit),
                    Edge::R(shared_off + ((i2 + 2) % 10) * shared_unit, shared_unit),
                ]
            }
            1 => {
                face.v = [i4 + 1, i4 + 6, (i4 + 1) % 5 + 1];
                face.e = [
                    Edge::F(shared_off + (i2 + 10) * shared_unit, shared_unit),
                    Edge::R(shared_off + (i2 + 11) * shared_unit, shared_unit),
                    Edge::R(shared_off + (i2 + 1) * shared_unit, shared_unit),
                ]
            }
            2 => {
                face.v = [(i4 + 1) % 5 + 1, i4 + 6, (i4 + 1) % 5 + 6];
                face.e = [
                    Edge::F(shared_off + (i2 + 10) * shared_unit, shared_unit),
                    Edge::R(shared_off + (i2 + 19) * shared_unit, shared_unit),
                    Edge::R(shared_off + ((i2 + 1) % 10 + 10) * shared_unit, shared_unit),
                ]
            }
            _ => {
                face.v = [(i4 + 1) % 5 + 6, i4 + 6, 11];
                face.e = [
                    Edge::F(shared_off + (i2 + 19) * shared_unit, shared_unit),
                    Edge::F(shared_off + (i2 + 20) * shared_unit, shared_unit),
                    Edge::R(shared_off + ((i2 + 2) % 10 + 20) * shared_unit, shared_unit),
                ]
            }
        }
    }

    // Slice forwarding edges only
    for f in faces.iter() {
        for (i, off, len) in f.e.iter().enumerate().filter_map(|(i, e)| match e {
            Edge::F(j, k) => Some((i, *j, *k)),
            Edge::R(_, _) => None,
        }) {
            cut_arc_into_pow2(buf, off, len, f.v[i], f.v[(i + 1) % 3]);
        }
    }

    faces
}

pub fn create_icosphere(radius: f32, division: usize, color: Color) -> Mesh {
    if division == 0 {
        return create_icosahedron(radius, color, None, None);
    }
    // Vertex order: [Seed(V), Shared(E * (2^d - 1)), Inner(F * sum of 1..=2^d - 2)]
    // where V: # of vertices, E: # of edges, F: # of faces, d: division
    // Shared: edge0(Va, Vb, Vc, ...), edge1, ..., edge29
    const V: usize = 12;
    const E: usize = 30;
    const F: usize = 20;
    let d = division;
    let shared_unit = (1 << d) - 1;
    let inner_unit = ((1 << d) - 2) * (1 + ((1 << d) - 2)) / 2;
    let (seed_len, shared_len, inner_len) = (V, E * shared_unit, F * inner_unit);
    let (shared_off, inner_off) = (seed_len, seed_len + shared_len);
    let vertex_len = seed_len + shared_len + inner_len;
    let index_len = 60 * 4_usize.pow(d as u32);

    let primitive = create_icosahedron(1.0, color, Some(vertex_len), Some(index_len));
    let positions: &Vec<Position> = primitive.get_position().unwrap().into();
    let mut positions = positions.clone();
    let mut indices = Vec::new();

    positions.resize(vertex_len, Position::default());

    // cut edges
    let faces = cut_icosahedron_edges(&mut positions, shared_off, shared_unit);

    // Divide each face
    fn divide(
        positions: &mut Vec<Position>,
        indices: &mut Vec<u32>,
        f: Face,
        d: usize,
        off: usize,
        len: usize,
    ) {
        if d == 0 {
            indices.extend(f.v.into_iter().map(|i| i as u32));
            return;
        }
        let unwrap = |e: Edge| match e {
            Edge::F(off, _) | Edge::R(off, _) => off,
        };
        let make_half_edge = |e: Edge, front: bool| match (e, front) {
            (Edge::F(off, len), true) => Edge::F(off, len / 2),
            (Edge::F(off, len), false) => Edge::F(off + len / 2 + 1, len / 2),
            (Edge::R(off, len), false) => Edge::R(off, len / 2),
            (Edge::R(off, len), true) => Edge::R(off + len / 2 + 1, len / 2),
        };
        let half = (1 << (d - 1)) - 1;
        let halfs = [
            unwrap(f.e[0]) + half,
            unwrap(f.e[1]) + half,
            unwrap(f.e[2]) + half,
        ];
        let half_offs = [off, off + half, off + 2 * half];
        let mut faces = [Face::default(); 4];
        for i in 0..3 {
            cut_arc_into_pow2(positions, half_offs[i], half, halfs[i], halfs[(i + 5) % 3]);
            faces[i] = Face {
                v: [f.v[i], halfs[i], halfs[(i + 5) % 3]],
                e: [
                    make_half_edge(f.e[i], true),
                    Edge::F(half_offs[i], half),
                    make_half_edge(f.e[(i + 5) % 3], false),
                ],
            }
        }
        faces[3] = Face {
            v: halfs,
            e: [
                Edge::R(half_offs[1], half),
                Edge::R(half_offs[2], half),
                Edge::R(half_offs[0], half),
            ],
        };
        let new_off = off + 3 * half;
        let new_len = (len - 3 * half) / 4;
        for (i, face) in faces.into_iter().enumerate() {
            divide(
                positions,
                indices,
                face,
                d - 1,
                new_off + i * new_len,
                new_len,
            );
        }
    }
    for (i, face) in faces.into_iter().enumerate() {
        divide(
            &mut positions,
            &mut indices,
            face,
            d,
            inner_off + i * inner_unit,
            inner_unit,
        );
    }

    // Copy positions into normals, and then adapt the radius
    let normals = positions.clone();
    for pos in positions.iter_mut() {
        *pos *= radius;
    }

    let colors = vec![color; positions.len()];

    Mesh::new()
        .with_position(positions.into())
        .with_normal(normals.into())
        .with_color(colors.into())
        .with_indices(indices)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitive::transform::{rotate_z, scale};
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    const EPS: f32 = 1e-6;

    #[wasm_bindgen_test]
    fn test_create_circle_returns_circle() {
        let center = Vector::<f32, 2>::new(1.0, 2.0);
        let radius = 1.0;
        let vertices = 16;
        let primitive = create_circle(center, radius, vertices, color::DEFAULT);
        let positions: &Vec<Position> = primitive.get_position().unwrap().into();
        assert_eq!(vertices, positions.len() as u32);
        assert!(positions
            .iter()
            .map(|pos| pos.dist(Position::from_vec_f32(center, [0.0, 0.0, 0.0, 1.0])))
            .all(|d| (d - radius).abs() < EPS));
    }

    #[wasm_bindgen_test]
    fn test_create_icosahedron_vertices_are_on_exact_positions() {
        for radius in [0.5, 1.0, 2.0] {
            let a = 1.0 / 5_f32.sqrt(); // 1 / 5^0.5
            let b = 2.0 * a; // 2 / 5^0.5
            let c = (1.0 - a) / 2.0; // (1 - 1 / 5^0.5) / 2
            let d = ((1.0 + a) / 2.0).sqrt(); // ((1 + 1 / 5^0.5) / 2)^0.5
            let e = (-1.0 - a) / 2.0; // (-1 - 1 / 5^0.5) / 2
            let f = c.sqrt(); // ((1 - 1 / 5^0.5) / 2)^0.5
            let mut expect = [
                Position::new(0.0, 1.0, 0.0),
                Position::new(b, a, 0.0),
                Position::new(c, a, -d),
                Position::new(e, a, -f),
                Position::new(e, a, f),
                Position::new(c, a, d),
                Position::new(-e, -a, -f),
                Position::new(-c, -a, -d),
                Position::new(-b, -a, 0.0),
                Position::new(-c, -a, d),
                Position::new(-e, -a, f),
                Position::new(0.0, -1.0, 0.0),
            ];
            for exp_v in expect.iter_mut() {
                *exp_v = &scale(radius, radius, radius) * (*exp_v);
            }
            let primitive = create_icosahedron(radius, color::DEFAULT, None, None);
            let positions: &Vec<Position> = primitive.get_position().unwrap().into();
            let indices = primitive.get_indices();
            assert_eq!(expect.len(), positions.len());
            assert_eq!(60, indices.len());
            assert!(positions
                .iter()
                .zip(expect.iter())
                .all(|(res, exp)| res.iter().zip(exp.iter()).all(|(x, y)| (x - y).abs() < EPS)));
        }
    }

    #[wasm_bindgen_test]
    #[rustfmt::skip]
    fn test_cut_arc_into_pow2_from_2_to_16() {
        for n in [2, 4, 8, 16] {
            let radius = 1.0;
            let mut buf = vec![Position::default(); n + 1];
            let rot_y = rotate_y(-radian::FRAC_PI_4);
            let s = Position::new(radius, 0.0, 0.0);
            let e = Position::new(0.0, radius, 0.0);
            (buf[0], buf[1]) = (&rot_y * s, e);

            let mut expect = buf.clone();
            let theta = radian::FRAC_PI_2 / n as f32;
            for i in 1..n {
                expect[i + 1] = (&(&rot_y * &rotate_z(theta * i as f32)) * s).into();
            }

            cut_arc_into_pow2(&mut buf, 2, n - 1, 0, 1);
            assert!(buf
                .iter()
                .zip(expect.iter())
                .all(|(res, exp)| res.iter().zip(exp.iter()).all(|(x, y)| (x - y).abs() < EPS)));
        }
    }

    #[wasm_bindgen_test]
    fn test_cut_icosahedron_edges_makes_the_same_sized_pieces() {
        for d in 0..=5 {
            const V: usize = 12;
            const E: usize = 30;
            let shared_unit = (1 << d) - 1;
            let (seed_len, shared_len) = (V, E * shared_unit);
            let shared_off = seed_len;
            let vertex_len = seed_len + shared_len;
            let primitive = create_icosahedron(1.0, color::DEFAULT, Some(vertex_len), None);
            let positions: &Vec<Position> = primitive.get_position().unwrap().into();
            let mut buf = positions.clone();

            buf.resize(seed_len + shared_len, Position::default());
            let faces = cut_icosahedron_edges(&mut buf, shared_off, shared_unit);
            let (mut low, mut high) = (f32::MAX, f32::MIN);
            for f in faces {
                for i in 0..3 {
                    let mut positions: Vec<Position> = vec![buf[f.v[i]]];
                    match f.e[i] {
                        Edge::F(si, len) => {
                            for j in si..si + len {
                                positions.push(buf[j]);
                            }
                        }
                        Edge::R(si, len) => {
                            for j in (si..si + len).rev() {
                                positions.push(buf[j]);
                            }
                        }
                    }
                    positions.push(buf[f.v[(i + 1) % 3]]);
                    for win in positions.windows(2) {
                        let d = win[0].dist(win[1]);
                        low = low.min(d);
                        high = high.max(d);
                    }
                }
            }
            assert!(high - low < EPS);
        }
    }

    #[wasm_bindgen_test]
    #[rustfmt::skip]
    fn test_create_icosphere_returns_sphere_with_division_from_0_to_4() {
        for (radius, division, vertex_num, index_num) in [
            (1.0, 0,   12,   20 * 3),
            (1.0, 1,   42,   80 * 3),
            (1.0, 2,  162,  320 * 3),
            (1.0, 3,  642, 1280 * 3),
            (1.0, 4, 2562, 5120 * 3),
            (0.5, 2,  162,  320 * 3),
            (2.0, 2,  162,  320 * 3),
        ] {
            let primitive = create_icosphere(radius, division, color::DEFAULT);
            let positions: &Vec<Position> = primitive.get_position().unwrap().into();
            let indices = primitive.get_indices();
            assert_eq!(vertex_num, positions.len());
            assert_eq!(index_num, indices.len());
            assert!(positions
                .iter()
                .all(|p| (p.norm_l2() - radius).abs() < EPS),
                "radius: {radius}, division: {division}"
            );
        }
    }
}
