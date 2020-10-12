import { createMuiTheme } from '@material-ui/core/styles';
import { red, blue, grey, blueGrey, lightBlue, green } from '@material-ui/core/colors';

// Create a theme instance.
const theme = createMuiTheme({
  palette: {
    primary: {
      main: lightBlue.A700,
    },
    secondary: {
      main: lightBlue.A100
    },
    error: {
      main: red.A400,
    },
    background: {
      default: '#fff',
    },
  },
});

export default theme;