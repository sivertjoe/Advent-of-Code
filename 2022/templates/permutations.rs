fn unique_permutations<T: Clone>(items: Vec<T>) -> Vec<Vec<T>>
where
    T: Ord,
{
    if items.len() == 1
    {
        vec![items]
    }
    else
    {
        let mut output: Vec<Vec<T>> = vec![];

        let mut unique_items = items.clone();
        unique_items.sort();
        unique_items.dedup();
        for first in unique_items
        {
            let mut remaining_elements = items.clone();

            let index = remaining_elements.iter().position(|x| *x == first).unwrap();
            remaining_elements.remove(index);

            for mut permutation in unique_permutations(remaining_elements)
            {
                permutation.insert(0, first.clone());
                output.push(permutation);
            }
        }
        output
    }
}
