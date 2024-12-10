use std::fs::read_to_string;

#[derive(Copy, Clone)]
struct Chunk {
    size: usize,
    index: Option<usize>,
}

fn to_blocks(diskmap: &str) -> Vec<isize> {
    diskmap
        .chars()
        .enumerate()
        .flat_map(|(i, c)| {
            let len = c.to_string().parse::<usize>().unwrap();
            if i % 2 == 0 {
                vec![i as isize / 2; len]
            } else {
                vec![-1; len]
            }
        })
        .collect()
}

fn to_chunks(diskmap: &str) -> Vec<Chunk> {
    diskmap
        .chars()
        .enumerate()
        .map(|(i, c)| {
            let len = c.to_string().parse::<usize>().unwrap();
            if i % 2 == 0 {
                Chunk {
                    size: len,
                    index: Some(i / 2),
                }
            } else {
                Chunk {
                    size: len,
                    index: None,
                }
            }
        })
        .collect()
}

fn to_block_compact(blocks: &[isize]) -> Vec<usize> {
    let replace: Vec<&isize> = blocks.iter().filter(|c| **c != -1).rev().collect();
    let mut compact: Vec<usize> = Default::default();
    let mut moved = 0;
    for &c in blocks.iter() {
        if c == -1 {
            compact.push(**replace.get(moved).unwrap() as usize);
            moved += 1;
        } else {
            compact.push(c as usize);
        }
    }
    compact[..compact.len() - moved].to_vec()
}

fn to_chunk_compact(chunks: &[Chunk]) -> Vec<Chunk> {
    let mut working: Vec<Chunk> = chunks.to_vec();
    let mut files: Vec<Chunk> = Default::default();

    while let Some(file) = working.pop() {
        if file.index.is_some() {
            match working
                .iter()
                .position(|chunk| chunk.size >= file.size && chunk.index.is_none())
            {
                Some(p) => {
                    let gap = working[p];
                    working[p] = file;
                    if gap.size > file.size {
                        working.insert(
                            p + 1,
                            Chunk {
                                size: gap.size - file.size,
                                index: None,
                            },
                        );
                    }

                    files.push(Chunk {
                        size: file.size,
                        index: None,
                    });
                }
                None => files.push(file),
            }
        } else {
            files.push(file);
        }
    }
    files.reverse();

    files
}

fn chunks_to_blocks(files: &[Chunk]) -> Vec<usize> {
    files
        .iter()
        .flat_map(|chunk| {
            if chunk.index.is_none() {
                vec![0; chunk.size]
            } else {
                vec![chunk.index.unwrap(); chunk.size]
            }
        })
        .collect()
}

fn checksum(compact: &[usize]) -> usize {
    compact.iter().enumerate().map(|(i, &c)| i * c).sum()
}

fn part1(diskmap: &str) -> usize {
    let blocks = to_blocks(diskmap);
    let compact = to_block_compact(&blocks);
    checksum(&compact)
}

fn part2(diskmap: &str) -> usize {
    let chunks = to_chunks(diskmap);
    let compact = to_chunk_compact(&chunks);
    let blocks = chunks_to_blocks(&compact);
    checksum(&blocks)
}

fn main() {
    let file = "./inputs/2024-12-09.txt";
    // let file = "./inputs/test.txt";
    let diskmap = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    println!("part1: {}", part1(&diskmap));
    println!("part2: {:?}", part2(&diskmap));
}
// part1: 6463499258318
// part2: 6493634986625
