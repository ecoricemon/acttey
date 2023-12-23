fn bar(a: u32) {
    #[ID(0)]
    #[vertex]
    {
        #[ID(1)]
        #[fragment]
        {}
    }
    #[ID(2)]
    #[compute]
    {
        #[ID(1)]
        #[fragment]
        {}
    }
}
