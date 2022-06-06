function withOpacityValue(variable) {
  return ({ opacityValue }) => {
    if (opacityValue === undefined) {
      return `hsl(var(${variable}))`;
    }
    return `hsla(var(${variable}), ${opacityValue})`;
  };
}

module.exports = {
  content: ["./index.html", "./src/**/*.{vue,js,ts,jsx,tsx,elm}"],
  theme: {
    colors: {
      //& Surface
      "surface-0": withOpacityValue("--clr-surface-0-alpha"),
      // & Text
      "text-0": withOpacityValue("--clr-text-0-alpha"),
      // & Brand
      "accent-0": withOpacityValue("--clr-brand-0-alpha"),
    },
    extend: {},
  },
  plugins: [],
  corePlugins: {
    preflight: false,
  },
};
