import Footer from "./footer";
import Navbar from "./navbar";
import { Container, Grid, Box } from "@material-ui/core";
import Header from "./header";

export default function Layout(props) {
  return (
    <Container maxWidth="md">
      <Box my={4}>
        <Header></Header>
        <Grid
          spacing={3}
          container
          direction="row"
          justify="center"
          alignItems="center"
        >
          <Grid item xs={12}>
            <Navbar></Navbar>
          </Grid>
          <Grid item xs={12}>
            {props.children}
          </Grid>
          <Grid
            item
            xs={12}
          >
            <Footer></Footer>
          </Grid>
        </Grid>
      </Box>
    </Container>
  );
}
